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
    , dClientARN

    -- * Response
    , DeleteLunaClientResponse
    -- ** Response constructor
    , deleteLunaClientResponse
    -- ** Response lenses
    , dlcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dClientARN'
newtype DeleteLunaClient = DeleteLunaClient'
    { _dClientARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLunaClient' smart constructor.
deleteLunaClient :: Text -> DeleteLunaClient
deleteLunaClient pClientARN_ =
    DeleteLunaClient'
    { _dClientARN = pClientARN_
    }

-- | The ARN of the client to delete.
dClientARN :: Lens' DeleteLunaClient Text
dClientARN = lens _dClientARN (\ s a -> s{_dClientARN = a});

instance AWSRequest DeleteLunaClient where
        type Sv DeleteLunaClient = CloudHSM
        type Rs DeleteLunaClient = DeleteLunaClientResponse
        request = postJSON "DeleteLunaClient"
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
          = object ["ClientArn" .= _dClientARN]

instance ToPath DeleteLunaClient where
        toPath = const "/"

instance ToQuery DeleteLunaClient where
        toQuery = const mempty

-- | /See:/ 'deleteLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrsStatus'
newtype DeleteLunaClientResponse = DeleteLunaClientResponse'
    { _dlcrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLunaClientResponse' smart constructor.
deleteLunaClientResponse :: Int -> DeleteLunaClientResponse
deleteLunaClientResponse pStatus_ =
    DeleteLunaClientResponse'
    { _dlcrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dlcrsStatus :: Lens' DeleteLunaClientResponse Int
dlcrsStatus = lens _dlcrsStatus (\ s a -> s{_dlcrsStatus = a});
