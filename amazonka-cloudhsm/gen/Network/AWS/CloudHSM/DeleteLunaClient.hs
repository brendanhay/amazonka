{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteLunaClient.html>
module Network.AWS.CloudHSM.DeleteLunaClient
    (
    -- * Request
      DeleteLunaClient
    -- ** Request constructor
    , deleteLunaClient
    -- ** Request lenses
    , delClientARN

    -- * Response
    , DeleteLunaClientResponse
    -- ** Response constructor
    , deleteLunaClientResponse
    -- ** Response lenses
    , dlcrStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delClientARN'
newtype DeleteLunaClient = DeleteLunaClient'
    { _delClientARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLunaClient' smart constructor.
deleteLunaClient :: Text -> DeleteLunaClient
deleteLunaClient pClientARN =
    DeleteLunaClient'
    { _delClientARN = pClientARN
    }

-- | The ARN of the client to delete.
delClientARN :: Lens' DeleteLunaClient Text
delClientARN = lens _delClientARN (\ s a -> s{_delClientARN = a});

instance AWSRequest DeleteLunaClient where
        type Sv DeleteLunaClient = CloudHSM
        type Rs DeleteLunaClient = DeleteLunaClientResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteLunaClientResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteLunaClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteLunaClient" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLunaClient where
        toJSON DeleteLunaClient'{..}
          = object ["ClientArn" .= _delClientARN]

instance ToPath DeleteLunaClient where
        toPath = const "/"

instance ToQuery DeleteLunaClient where
        toQuery = const mempty

-- | /See:/ 'deleteLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrStatus'
newtype DeleteLunaClientResponse = DeleteLunaClientResponse'
    { _dlcrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLunaClientResponse' smart constructor.
deleteLunaClientResponse :: Int -> DeleteLunaClientResponse
deleteLunaClientResponse pStatus =
    DeleteLunaClientResponse'
    { _dlcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dlcrStatus :: Lens' DeleteLunaClientResponse Int
dlcrStatus = lens _dlcrStatus (\ s a -> s{_dlcrStatus = a});
