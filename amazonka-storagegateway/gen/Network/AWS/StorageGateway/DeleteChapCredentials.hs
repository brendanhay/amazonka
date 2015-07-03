{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation deletes Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target and initiator pair.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteChapCredentials.html>
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Request
      DeleteChapCredentials
    -- ** Request constructor
    , deleteChapCredentials
    -- ** Request lenses
    , dTargetARN
    , dInitiatorName

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response constructor
    , deleteChapCredentialsResponse
    -- ** Response lenses
    , delTargetARN
    , delInitiatorName
    , delStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   DeleteChapCredentialsInput$InitiatorName
-- -   DeleteChapCredentialsInput$TargetARN
--
-- /See:/ 'deleteChapCredentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dTargetARN'
--
-- * 'dInitiatorName'
data DeleteChapCredentials = DeleteChapCredentials'
    { _dTargetARN     :: !Text
    , _dInitiatorName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteChapCredentials' smart constructor.
deleteChapCredentials :: Text -> Text -> DeleteChapCredentials
deleteChapCredentials pTargetARN pInitiatorName =
    DeleteChapCredentials'
    { _dTargetARN = pTargetARN
    , _dInitiatorName = pInitiatorName
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dTargetARN :: Lens' DeleteChapCredentials Text
dTargetARN = lens _dTargetARN (\ s a -> s{_dTargetARN = a});

-- | The iSCSI initiator that connects to the target.
dInitiatorName :: Lens' DeleteChapCredentials Text
dInitiatorName = lens _dInitiatorName (\ s a -> s{_dInitiatorName = a});

instance AWSRequest DeleteChapCredentials where
        type Sv DeleteChapCredentials = StorageGateway
        type Rs DeleteChapCredentials =
             DeleteChapCredentialsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteChapCredentialsResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "InitiatorName") <*>
                     (pure (fromEnum s)))

instance ToHeaders DeleteChapCredentials where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteChapCredentials" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteChapCredentials where
        toJSON DeleteChapCredentials'{..}
          = object
              ["TargetARN" .= _dTargetARN,
               "InitiatorName" .= _dInitiatorName]

instance ToPath DeleteChapCredentials where
        toPath = const "/"

instance ToQuery DeleteChapCredentials where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'deleteChapCredentialsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delTargetARN'
--
-- * 'delInitiatorName'
--
-- * 'delStatus'
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
    { _delTargetARN     :: !(Maybe Text)
    , _delInitiatorName :: !(Maybe Text)
    , _delStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteChapCredentialsResponse' smart constructor.
deleteChapCredentialsResponse :: Int -> DeleteChapCredentialsResponse
deleteChapCredentialsResponse pStatus =
    DeleteChapCredentialsResponse'
    { _delTargetARN = Nothing
    , _delInitiatorName = Nothing
    , _delStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the target.
delTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
delTargetARN = lens _delTargetARN (\ s a -> s{_delTargetARN = a});

-- | The iSCSI initiator that connects to the target.
delInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
delInitiatorName = lens _delInitiatorName (\ s a -> s{_delInitiatorName = a});

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteChapCredentialsResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
