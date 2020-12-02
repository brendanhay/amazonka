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
-- Module      : Network.AWS.DirectoryService.DeleteTrust
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your Microsoft AD in the AWS cloud and an external domain.
--
--
module Network.AWS.DirectoryService.DeleteTrust
    (
    -- * Creating a Request
      deleteTrust
    , DeleteTrust
    -- * Request Lenses
    , dtDeleteAssociatedConditionalForwarder
    , dtTrustId

    -- * Destructuring the Response
    , deleteTrustResponse
    , DeleteTrustResponse
    -- * Response Lenses
    , dttrsTrustId
    , dttrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.
--
--
--
-- /See:/ 'deleteTrust' smart constructor.
data DeleteTrust = DeleteTrust'
  { _dtDeleteAssociatedConditionalForwarder :: !(Maybe Bool)
  , _dtTrustId                              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTrust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDeleteAssociatedConditionalForwarder' - Delete a conditional forwarder as part of a DeleteTrustRequest.
--
-- * 'dtTrustId' - The Trust ID of the trust relationship to be deleted.
deleteTrust
    :: Text -- ^ 'dtTrustId'
    -> DeleteTrust
deleteTrust pTrustId_ =
  DeleteTrust'
    {_dtDeleteAssociatedConditionalForwarder = Nothing, _dtTrustId = pTrustId_}


-- | Delete a conditional forwarder as part of a DeleteTrustRequest.
dtDeleteAssociatedConditionalForwarder :: Lens' DeleteTrust (Maybe Bool)
dtDeleteAssociatedConditionalForwarder = lens _dtDeleteAssociatedConditionalForwarder (\ s a -> s{_dtDeleteAssociatedConditionalForwarder = a})

-- | The Trust ID of the trust relationship to be deleted.
dtTrustId :: Lens' DeleteTrust Text
dtTrustId = lens _dtTrustId (\ s a -> s{_dtTrustId = a})

instance AWSRequest DeleteTrust where
        type Rs DeleteTrust = DeleteTrustResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTrustResponse' <$>
                   (x .?> "TrustId") <*> (pure (fromEnum s)))

instance Hashable DeleteTrust where

instance NFData DeleteTrust where

instance ToHeaders DeleteTrust where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteTrust" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTrust where
        toJSON DeleteTrust'{..}
          = object
              (catMaybes
                 [("DeleteAssociatedConditionalForwarder" .=) <$>
                    _dtDeleteAssociatedConditionalForwarder,
                  Just ("TrustId" .= _dtTrustId)])

instance ToPath DeleteTrust where
        toPath = const "/"

instance ToQuery DeleteTrust where
        toQuery = const mempty

-- | The result of a DeleteTrust request.
--
--
--
-- /See:/ 'deleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'
  { _dttrsTrustId        :: !(Maybe Text)
  , _dttrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTrustResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttrsTrustId' - The Trust ID of the trust relationship that was deleted.
--
-- * 'dttrsResponseStatus' - -- | The response status code.
deleteTrustResponse
    :: Int -- ^ 'dttrsResponseStatus'
    -> DeleteTrustResponse
deleteTrustResponse pResponseStatus_ =
  DeleteTrustResponse'
    {_dttrsTrustId = Nothing, _dttrsResponseStatus = pResponseStatus_}


-- | The Trust ID of the trust relationship that was deleted.
dttrsTrustId :: Lens' DeleteTrustResponse (Maybe Text)
dttrsTrustId = lens _dttrsTrustId (\ s a -> s{_dttrsTrustId = a})

-- | -- | The response status code.
dttrsResponseStatus :: Lens' DeleteTrustResponse Int
dttrsResponseStatus = lens _dttrsResponseStatus (\ s a -> s{_dttrsResponseStatus = a})

instance NFData DeleteTrustResponse where
