{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
--
--
module Network.AWS.ServiceCatalog.DescribeCopyProductStatus
    (
    -- * Creating a Request
      describeCopyProductStatus
    , DescribeCopyProductStatus
    -- * Request Lenses
    , dcpsAcceptLanguage
    , dcpsCopyProductToken

    -- * Destructuring the Response
    , describeCopyProductStatusResponse
    , DescribeCopyProductStatusResponse
    -- * Response Lenses
    , dcpsrsTargetProductId
    , dcpsrsCopyProductStatus
    , dcpsrsStatusDetail
    , dcpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { _dcpsAcceptLanguage   :: !(Maybe Text)
  , _dcpsCopyProductToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCopyProductStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dcpsCopyProductToken' - The token for the copy product operation. This token is returned by 'CopyProduct' .
describeCopyProductStatus
    :: Text -- ^ 'dcpsCopyProductToken'
    -> DescribeCopyProductStatus
describeCopyProductStatus pCopyProductToken_ =
  DescribeCopyProductStatus'
    {_dcpsAcceptLanguage = Nothing, _dcpsCopyProductToken = pCopyProductToken_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dcpsAcceptLanguage :: Lens' DescribeCopyProductStatus (Maybe Text)
dcpsAcceptLanguage = lens _dcpsAcceptLanguage (\ s a -> s{_dcpsAcceptLanguage = a})

-- | The token for the copy product operation. This token is returned by 'CopyProduct' .
dcpsCopyProductToken :: Lens' DescribeCopyProductStatus Text
dcpsCopyProductToken = lens _dcpsCopyProductToken (\ s a -> s{_dcpsCopyProductToken = a})

instance AWSRequest DescribeCopyProductStatus where
        type Rs DescribeCopyProductStatus =
             DescribeCopyProductStatusResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCopyProductStatusResponse' <$>
                   (x .?> "TargetProductId") <*>
                     (x .?> "CopyProductStatus")
                     <*> (x .?> "StatusDetail")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCopyProductStatus where

instance NFData DescribeCopyProductStatus where

instance ToHeaders DescribeCopyProductStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeCopyProductStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCopyProductStatus where
        toJSON DescribeCopyProductStatus'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dcpsAcceptLanguage,
                  Just ("CopyProductToken" .= _dcpsCopyProductToken)])

instance ToPath DescribeCopyProductStatus where
        toPath = const "/"

instance ToQuery DescribeCopyProductStatus where
        toQuery = const mempty

-- | /See:/ 'describeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { _dcpsrsTargetProductId   :: !(Maybe Text)
  , _dcpsrsCopyProductStatus :: !(Maybe CopyProductStatus)
  , _dcpsrsStatusDetail      :: !(Maybe Text)
  , _dcpsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCopyProductStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpsrsTargetProductId' - The identifier of the copied product.
--
-- * 'dcpsrsCopyProductStatus' - The status of the copy product operation.
--
-- * 'dcpsrsStatusDetail' - The status message.
--
-- * 'dcpsrsResponseStatus' - -- | The response status code.
describeCopyProductStatusResponse
    :: Int -- ^ 'dcpsrsResponseStatus'
    -> DescribeCopyProductStatusResponse
describeCopyProductStatusResponse pResponseStatus_ =
  DescribeCopyProductStatusResponse'
    { _dcpsrsTargetProductId = Nothing
    , _dcpsrsCopyProductStatus = Nothing
    , _dcpsrsStatusDetail = Nothing
    , _dcpsrsResponseStatus = pResponseStatus_
    }


-- | The identifier of the copied product.
dcpsrsTargetProductId :: Lens' DescribeCopyProductStatusResponse (Maybe Text)
dcpsrsTargetProductId = lens _dcpsrsTargetProductId (\ s a -> s{_dcpsrsTargetProductId = a})

-- | The status of the copy product operation.
dcpsrsCopyProductStatus :: Lens' DescribeCopyProductStatusResponse (Maybe CopyProductStatus)
dcpsrsCopyProductStatus = lens _dcpsrsCopyProductStatus (\ s a -> s{_dcpsrsCopyProductStatus = a})

-- | The status message.
dcpsrsStatusDetail :: Lens' DescribeCopyProductStatusResponse (Maybe Text)
dcpsrsStatusDetail = lens _dcpsrsStatusDetail (\ s a -> s{_dcpsrsStatusDetail = a})

-- | -- | The response status code.
dcpsrsResponseStatus :: Lens' DescribeCopyProductStatusResponse Int
dcpsrsResponseStatus = lens _dcpsrsResponseStatus (\ s a -> s{_dcpsrsResponseStatus = a})

instance NFData DescribeCopyProductStatusResponse
         where
