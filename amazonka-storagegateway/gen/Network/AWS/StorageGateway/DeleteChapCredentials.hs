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
-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair.
--
--
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Creating a Request
      deleteChapCredentials
    , DeleteChapCredentials
    -- * Request Lenses
    , dTargetARN
    , dInitiatorName

    -- * Destructuring the Response
    , deleteChapCredentialsResponse
    , DeleteChapCredentialsResponse
    -- * Response Lenses
    , drsTargetARN
    , drsInitiatorName
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'DeleteChapCredentialsInput$InitiatorName'
--
--     * 'DeleteChapCredentialsInput$TargetARN'
--
--
--
--
-- /See:/ 'deleteChapCredentials' smart constructor.
data DeleteChapCredentials = DeleteChapCredentials'
  { _dTargetARN     :: !Text
  , _dInitiatorName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChapCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTargetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- * 'dInitiatorName' - The iSCSI initiator that connects to the target.
deleteChapCredentials
    :: Text -- ^ 'dTargetARN'
    -> Text -- ^ 'dInitiatorName'
    -> DeleteChapCredentials
deleteChapCredentials pTargetARN_ pInitiatorName_ =
  DeleteChapCredentials'
    {_dTargetARN = pTargetARN_, _dInitiatorName = pInitiatorName_}


-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
dTargetARN :: Lens' DeleteChapCredentials Text
dTargetARN = lens _dTargetARN (\ s a -> s{_dTargetARN = a})

-- | The iSCSI initiator that connects to the target.
dInitiatorName :: Lens' DeleteChapCredentials Text
dInitiatorName = lens _dInitiatorName (\ s a -> s{_dInitiatorName = a})

instance AWSRequest DeleteChapCredentials where
        type Rs DeleteChapCredentials =
             DeleteChapCredentialsResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DeleteChapCredentialsResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "InitiatorName") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteChapCredentials where

instance NFData DeleteChapCredentials where

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
              (catMaybes
                 [Just ("TargetARN" .= _dTargetARN),
                  Just ("InitiatorName" .= _dInitiatorName)])

instance ToPath DeleteChapCredentials where
        toPath = const "/"

instance ToQuery DeleteChapCredentials where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'deleteChapCredentialsResponse' smart constructor.
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
  { _drsTargetARN      :: !(Maybe Text)
  , _drsInitiatorName  :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChapCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsTargetARN' - The Amazon Resource Name (ARN) of the target.
--
-- * 'drsInitiatorName' - The iSCSI initiator that connects to the target.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteChapCredentialsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteChapCredentialsResponse
deleteChapCredentialsResponse pResponseStatus_ =
  DeleteChapCredentialsResponse'
    { _drsTargetARN = Nothing
    , _drsInitiatorName = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the target.
drsTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
drsTargetARN = lens _drsTargetARN (\ s a -> s{_drsTargetARN = a})

-- | The iSCSI initiator that connects to the target.
drsInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
drsInitiatorName = lens _drsInitiatorName (\ s a -> s{_drsInitiatorName = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteChapCredentialsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteChapCredentialsResponse where
