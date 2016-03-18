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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your Microsoft AD in the
-- AWS cloud and an external domain.
module Network.AWS.DirectoryService.DeleteTrust
    (
    -- * Creating a Request
      deleteTrust
    , DeleteTrust
    -- * Request Lenses
    , dtTrustId

    -- * Destructuring the Response
    , deleteTrustResponse
    , DeleteTrustResponse
    -- * Response Lenses
    , delrsTrustId
    , delrsResponseStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Deletes the local side of an existing trust relationship between the
-- Microsoft AD in the AWS cloud and the external domain.
--
-- /See:/ 'deleteTrust' smart constructor.
newtype DeleteTrust = DeleteTrust'
    { _dtTrustId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTrust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTrustId'
deleteTrust
    :: Text -- ^ 'dtTrustId'
    -> DeleteTrust
deleteTrust pTrustId_ =
    DeleteTrust'
    { _dtTrustId = pTrustId_
    }

-- | The Trust ID of the trust relationship to be deleted.
dtTrustId :: Lens' DeleteTrust Text
dtTrustId = lens _dtTrustId (\ s a -> s{_dtTrustId = a});

instance AWSRequest DeleteTrust where
        type Rs DeleteTrust = DeleteTrustResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTrustResponse' <$>
                   (x .?> "TrustId") <*> (pure (fromEnum s)))

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
          = object (catMaybes [Just ("TrustId" .= _dtTrustId)])

instance ToPath DeleteTrust where
        toPath = const "/"

instance ToQuery DeleteTrust where
        toQuery = const mempty

-- | /See:/ 'deleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'
    { _delrsTrustId        :: !(Maybe Text)
    , _delrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTrustResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsTrustId'
--
-- * 'delrsResponseStatus'
deleteTrustResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteTrustResponse
deleteTrustResponse pResponseStatus_ =
    DeleteTrustResponse'
    { _delrsTrustId = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }

-- | The Trust ID of the trust relationship that was deleted.
delrsTrustId :: Lens' DeleteTrustResponse (Maybe Text)
delrsTrustId = lens _delrsTrustId (\ s a -> s{_delrsTrustId = a});

-- | The response status code.
delrsResponseStatus :: Lens' DeleteTrustResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});
