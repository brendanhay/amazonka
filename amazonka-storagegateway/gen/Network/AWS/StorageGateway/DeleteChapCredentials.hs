{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target and initiator pair.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteChapCredentials.html AWS API Reference> for DeleteChapCredentials.
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Creating a Request
      DeleteChapCredentials
    , deleteChapCredentials
    -- * Request Lenses
    , dTargetARN
    , dInitiatorName

    -- * Destructuring the Response
    , DeleteChapCredentialsResponse
    , deleteChapCredentialsResponse
    -- * Response Lenses
    , drsTargetARN
    , drsInitiatorName
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteChapCredentials' smart constructor.
deleteChapCredentials :: Text -> Text -> DeleteChapCredentials
deleteChapCredentials pTargetARN_ pInitiatorName_ =
    DeleteChapCredentials'
    { _dTargetARN = pTargetARN_
    , _dInitiatorName = pInitiatorName_
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
-- * 'drsTargetARN'
--
-- * 'drsInitiatorName'
--
-- * 'drsStatus'
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
    { _drsTargetARN     :: !(Maybe Text)
    , _drsInitiatorName :: !(Maybe Text)
    , _drsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteChapCredentialsResponse' smart constructor.
deleteChapCredentialsResponse :: Int -> DeleteChapCredentialsResponse
deleteChapCredentialsResponse pStatus_ =
    DeleteChapCredentialsResponse'
    { _drsTargetARN = Nothing
    , _drsInitiatorName = Nothing
    , _drsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the target.
drsTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
drsTargetARN = lens _drsTargetARN (\ s a -> s{_drsTargetARN = a});

-- | The iSCSI initiator that connects to the target.
drsInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
drsInitiatorName = lens _drsInitiatorName (\ s a -> s{_drsInitiatorName = a});

-- | Undocumented member.
drsStatus :: Lens' DeleteChapCredentialsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
