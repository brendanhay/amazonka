{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , delTargetARN
    , delInitiatorName

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response constructor
    , deleteChapCredentialsResponse
    -- ** Response lenses
    , dccrTargetARN
    , dccrInitiatorName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteChapCredentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delTargetARN'
--
-- * 'delInitiatorName'
data DeleteChapCredentials = DeleteChapCredentials'{_delTargetARN :: Text, _delInitiatorName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteChapCredentials' smart constructor.
deleteChapCredentials :: Text -> Text -> DeleteChapCredentials
deleteChapCredentials pTargetARN pInitiatorName = DeleteChapCredentials'{_delTargetARN = pTargetARN, _delInitiatorName = pInitiatorName};

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
delTargetARN :: Lens' DeleteChapCredentials Text
delTargetARN = lens _delTargetARN (\ s a -> s{_delTargetARN = a});

-- | The iSCSI initiator that connects to the target.
delInitiatorName :: Lens' DeleteChapCredentials Text
delInitiatorName = lens _delInitiatorName (\ s a -> s{_delInitiatorName = a});

instance AWSRequest DeleteChapCredentials where
        type Sv DeleteChapCredentials = StorageGateway
        type Rs DeleteChapCredentials =
             DeleteChapCredentialsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteChapCredentialsResponse' <$>
                   x .:> "TargetARN" <*> x .:> "InitiatorName")

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
              ["TargetARN" .= _delTargetARN,
               "InitiatorName" .= _delInitiatorName]

instance ToPath DeleteChapCredentials where
        toPath = const "/"

instance ToQuery DeleteChapCredentials where
        toQuery = const mempty

-- | /See:/ 'deleteChapCredentialsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrTargetARN'
--
-- * 'dccrInitiatorName'
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'{_dccrTargetARN :: Text, _dccrInitiatorName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteChapCredentialsResponse' smart constructor.
deleteChapCredentialsResponse :: Text -> Text -> DeleteChapCredentialsResponse
deleteChapCredentialsResponse pTargetARN pInitiatorName = DeleteChapCredentialsResponse'{_dccrTargetARN = pTargetARN, _dccrInitiatorName = pInitiatorName};

-- | The Amazon Resource Name (ARN) of the target.
dccrTargetARN :: Lens' DeleteChapCredentialsResponse Text
dccrTargetARN = lens _dccrTargetARN (\ s a -> s{_dccrTargetARN = a});

-- | The iSCSI initiator that connects to the target.
dccrInitiatorName :: Lens' DeleteChapCredentialsResponse Text
dccrInitiatorName = lens _dccrInitiatorName (\ s a -> s{_dccrInitiatorName = a});
