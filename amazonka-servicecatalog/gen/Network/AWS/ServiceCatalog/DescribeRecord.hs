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
-- Module      : Network.AWS.ServiceCatalog.DescribeRecord
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified request operation.
--
--
-- Use this operation after calling a request operation (for example, 'ProvisionProduct' , 'TerminateProvisionedProduct' , or 'UpdateProvisionedProduct' ).
--
module Network.AWS.ServiceCatalog.DescribeRecord
    (
    -- * Creating a Request
      describeRecord
    , DescribeRecord
    -- * Request Lenses
    , drAcceptLanguage
    , drPageToken
    , drPageSize
    , drId

    -- * Destructuring the Response
    , describeRecordResponse
    , DescribeRecordResponse
    -- * Response Lenses
    , drrsRecordDetail
    , drrsNextPageToken
    , drrsRecordOutputs
    , drrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeRecord' smart constructor.
data DescribeRecord = DescribeRecord'
  { _drAcceptLanguage :: !(Maybe Text)
  , _drPageToken      :: !(Maybe Text)
  , _drPageSize       :: !(Maybe Nat)
  , _drId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'drPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'drPageSize' - The maximum number of items to return with this call.
--
-- * 'drId' - The record identifier of the provisioned product. This identifier is returned by the request operation.
describeRecord
    :: Text -- ^ 'drId'
    -> DescribeRecord
describeRecord pId_ =
  DescribeRecord'
    { _drAcceptLanguage = Nothing
    , _drPageToken = Nothing
    , _drPageSize = Nothing
    , _drId = pId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
drAcceptLanguage :: Lens' DescribeRecord (Maybe Text)
drAcceptLanguage = lens _drAcceptLanguage (\ s a -> s{_drAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
drPageToken :: Lens' DescribeRecord (Maybe Text)
drPageToken = lens _drPageToken (\ s a -> s{_drPageToken = a})

-- | The maximum number of items to return with this call.
drPageSize :: Lens' DescribeRecord (Maybe Natural)
drPageSize = lens _drPageSize (\ s a -> s{_drPageSize = a}) . mapping _Nat

-- | The record identifier of the provisioned product. This identifier is returned by the request operation.
drId :: Lens' DescribeRecord Text
drId = lens _drId (\ s a -> s{_drId = a})

instance AWSRequest DescribeRecord where
        type Rs DescribeRecord = DescribeRecordResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRecordResponse' <$>
                   (x .?> "RecordDetail") <*> (x .?> "NextPageToken")
                     <*> (x .?> "RecordOutputs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRecord where

instance NFData DescribeRecord where

instance ToHeaders DescribeRecord where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeRecord" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRecord where
        toJSON DescribeRecord'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _drAcceptLanguage,
                  ("PageToken" .=) <$> _drPageToken,
                  ("PageSize" .=) <$> _drPageSize,
                  Just ("Id" .= _drId)])

instance ToPath DescribeRecord where
        toPath = const "/"

instance ToQuery DescribeRecord where
        toQuery = const mempty

-- | /See:/ 'describeRecordResponse' smart constructor.
data DescribeRecordResponse = DescribeRecordResponse'
  { _drrsRecordDetail   :: !(Maybe RecordDetail)
  , _drrsNextPageToken  :: !(Maybe Text)
  , _drrsRecordOutputs  :: !(Maybe [RecordOutput])
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRecordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRecordDetail' - Information about the product.
--
-- * 'drrsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'drrsRecordOutputs' - Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRecordResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRecordResponse
describeRecordResponse pResponseStatus_ =
  DescribeRecordResponse'
    { _drrsRecordDetail = Nothing
    , _drrsNextPageToken = Nothing
    , _drrsRecordOutputs = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }


-- | Information about the product.
drrsRecordDetail :: Lens' DescribeRecordResponse (Maybe RecordDetail)
drrsRecordDetail = lens _drrsRecordDetail (\ s a -> s{_drrsRecordDetail = a})

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
drrsNextPageToken :: Lens' DescribeRecordResponse (Maybe Text)
drrsNextPageToken = lens _drrsNextPageToken (\ s a -> s{_drrsNextPageToken = a})

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
drrsRecordOutputs :: Lens' DescribeRecordResponse [RecordOutput]
drrsRecordOutputs = lens _drrsRecordOutputs (\ s a -> s{_drrsRecordOutputs = a}) . _Default . _Coerce

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRecordResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRecordResponse where
