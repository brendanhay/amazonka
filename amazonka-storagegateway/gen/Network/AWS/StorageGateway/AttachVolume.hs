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
-- Module      : Network.AWS.StorageGateway.AttachVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects a volume to an iSCSI connection and then attaches the volume to the specified gateway. Detaching and attaching a volume enables you to recover your data from one gateway to a different gateway without creating a snapshot. It also makes it easier to move your volumes from an on-premises gateway to a gateway hosted on an Amazon EC2 instance.
--
--
module Network.AWS.StorageGateway.AttachVolume
    (
    -- * Creating a Request
      attachVolume
    , AttachVolume
    -- * Request Lenses
    , avDiskId
    , avTargetName
    , avGatewayARN
    , avVolumeARN
    , avNetworkInterfaceId

    -- * Destructuring the Response
    , attachVolumeResponse
    , AttachVolumeResponse
    -- * Response Lenses
    , avrsTargetARN
    , avrsVolumeARN
    , avrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | AttachVolumeInput
--
--
--
-- /See:/ 'attachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { _avDiskId             :: !(Maybe Text)
  , _avTargetName         :: !(Maybe Text)
  , _avGatewayARN         :: !Text
  , _avVolumeARN          :: !Text
  , _avNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avDiskId' - The unique device ID or other distinguishing data that identifies the local disk used to create the volume. This value is only required when you are attaching a stored volume.
--
-- * 'avTargetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- * 'avGatewayARN' - The Amazon Resource Name (ARN) of the gateway that you want to attach the volume to.
--
-- * 'avVolumeARN' - The Amazon Resource Name (ARN) of the volume to attach to the specified gateway.
--
-- * 'avNetworkInterfaceId' - The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway. Valid Values: A valid IP address.
attachVolume
    :: Text -- ^ 'avGatewayARN'
    -> Text -- ^ 'avVolumeARN'
    -> Text -- ^ 'avNetworkInterfaceId'
    -> AttachVolume
attachVolume pGatewayARN_ pVolumeARN_ pNetworkInterfaceId_ =
  AttachVolume'
    { _avDiskId = Nothing
    , _avTargetName = Nothing
    , _avGatewayARN = pGatewayARN_
    , _avVolumeARN = pVolumeARN_
    , _avNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | The unique device ID or other distinguishing data that identifies the local disk used to create the volume. This value is only required when you are attaching a stored volume.
avDiskId :: Lens' AttachVolume (Maybe Text)
avDiskId = lens _avDiskId (\ s a -> s{_avDiskId = a})

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
avTargetName :: Lens' AttachVolume (Maybe Text)
avTargetName = lens _avTargetName (\ s a -> s{_avTargetName = a})

-- | The Amazon Resource Name (ARN) of the gateway that you want to attach the volume to.
avGatewayARN :: Lens' AttachVolume Text
avGatewayARN = lens _avGatewayARN (\ s a -> s{_avGatewayARN = a})

-- | The Amazon Resource Name (ARN) of the volume to attach to the specified gateway.
avVolumeARN :: Lens' AttachVolume Text
avVolumeARN = lens _avVolumeARN (\ s a -> s{_avVolumeARN = a})

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway. Valid Values: A valid IP address.
avNetworkInterfaceId :: Lens' AttachVolume Text
avNetworkInterfaceId = lens _avNetworkInterfaceId (\ s a -> s{_avNetworkInterfaceId = a})

instance AWSRequest AttachVolume where
        type Rs AttachVolume = AttachVolumeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 AttachVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN") <*>
                     (pure (fromEnum s)))

instance Hashable AttachVolume where

instance NFData AttachVolume where

instance ToHeaders AttachVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.AttachVolume" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachVolume where
        toJSON AttachVolume'{..}
          = object
              (catMaybes
                 [("DiskId" .=) <$> _avDiskId,
                  ("TargetName" .=) <$> _avTargetName,
                  Just ("GatewayARN" .= _avGatewayARN),
                  Just ("VolumeARN" .= _avVolumeARN),
                  Just
                    ("NetworkInterfaceId" .= _avNetworkInterfaceId)])

instance ToPath AttachVolume where
        toPath = const "/"

instance ToQuery AttachVolume where
        toQuery = const mempty

-- | AttachVolumeOutput
--
--
--
-- /See:/ 'attachVolumeResponse' smart constructor.
data AttachVolumeResponse = AttachVolumeResponse'
  { _avrsTargetARN      :: !(Maybe Text)
  , _avrsVolumeARN      :: !(Maybe Text)
  , _avrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avrsTargetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name for the initiator that was used to connect to the target.
--
-- * 'avrsVolumeARN' - The Amazon Resource Name (ARN) of the volume that was attached to the gateway.
--
-- * 'avrsResponseStatus' - -- | The response status code.
attachVolumeResponse
    :: Int -- ^ 'avrsResponseStatus'
    -> AttachVolumeResponse
attachVolumeResponse pResponseStatus_ =
  AttachVolumeResponse'
    { _avrsTargetARN = Nothing
    , _avrsVolumeARN = Nothing
    , _avrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name for the initiator that was used to connect to the target.
avrsTargetARN :: Lens' AttachVolumeResponse (Maybe Text)
avrsTargetARN = lens _avrsTargetARN (\ s a -> s{_avrsTargetARN = a})

-- | The Amazon Resource Name (ARN) of the volume that was attached to the gateway.
avrsVolumeARN :: Lens' AttachVolumeResponse (Maybe Text)
avrsVolumeARN = lens _avrsVolumeARN (\ s a -> s{_avrsVolumeARN = a})

-- | -- | The response status code.
avrsResponseStatus :: Lens' AttachVolumeResponse Int
avrsResponseStatus = lens _avrsResponseStatus (\ s a -> s{_avrsResponseStatus = a})

instance NFData AttachVolumeResponse where
