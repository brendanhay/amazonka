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
-- Module      : Network.AWS.EC2.RegisterImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AMI. When you\'re creating an AMI, this is the final step
-- you must complete before you can launch an instance from the AMI. This
-- step is required if you\'re creating an instance store-backed Linux or
-- Windows AMI. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-instance-store.html Creating an Instance Store-Backed Linux AMI>
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/Creating_InstanceStoreBacked_WinAMI.html Creating an Instance Store-Backed Windows AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For Amazon EBS-backed instances, CreateImage creates and registers the
-- AMI in a single request, so you don\'t have to register the AMI
-- yourself.
--
-- You can also use 'RegisterImage' to create an Amazon EBS-backed AMI from
-- a snapshot of a root device volume. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-launch-snapshot.html Launching an Instance from a Backup>
-- in the /Amazon Elastic Compute Cloud User Guide/. Note that although you
-- can create a Windows AMI from a snapshot, you can\'t launch an instance
-- from the AMI - use the CreateImage command instead.
--
-- If needed, you can deregister an AMI at any time. Any modifications you
-- make to an AMI backed by an instance store volume invalidates its
-- registration. If you make changes to an image, deregister the previous
-- image and register the new image.
--
-- You can\'t register an image where a secondary (non-root) snapshot has
-- AWS Marketplace product codes.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RegisterImage.html AWS API Reference> for RegisterImage.
module Network.AWS.EC2.RegisterImage
    (
    -- * Creating a Request
      registerImage
    , RegisterImage
    -- * Request Lenses
    , riVirtualizationType
    , riImageLocation
    , riRAMDiskId
    , riKernelId
    , riRootDeviceName
    , riSRIOVNetSupport
    , riArchitecture
    , riBlockDeviceMappings
    , riDryRun
    , riDescription
    , riName

    -- * Destructuring the Response
    , registerImageResponse
    , RegisterImageResponse
    -- * Response Lenses
    , rirsImageId
    , rirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerImage' smart constructor.
data RegisterImage = RegisterImage'
    { _riVirtualizationType  :: !(Maybe Text)
    , _riImageLocation       :: !(Maybe Text)
    , _riRAMDiskId           :: !(Maybe Text)
    , _riKernelId            :: !(Maybe Text)
    , _riRootDeviceName      :: !(Maybe Text)
    , _riSRIOVNetSupport     :: !(Maybe Text)
    , _riArchitecture        :: !(Maybe ArchitectureValues)
    , _riBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _riDryRun              :: !(Maybe Bool)
    , _riDescription         :: !(Maybe Text)
    , _riName                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riVirtualizationType'
--
-- * 'riImageLocation'
--
-- * 'riRAMDiskId'
--
-- * 'riKernelId'
--
-- * 'riRootDeviceName'
--
-- * 'riSRIOVNetSupport'
--
-- * 'riArchitecture'
--
-- * 'riBlockDeviceMappings'
--
-- * 'riDryRun'
--
-- * 'riDescription'
--
-- * 'riName'
registerImage
    :: Text -- ^ 'riName'
    -> RegisterImage
registerImage pName_ =
    RegisterImage'
    { _riVirtualizationType = Nothing
    , _riImageLocation = Nothing
    , _riRAMDiskId = Nothing
    , _riKernelId = Nothing
    , _riRootDeviceName = Nothing
    , _riSRIOVNetSupport = Nothing
    , _riArchitecture = Nothing
    , _riBlockDeviceMappings = Nothing
    , _riDryRun = Nothing
    , _riDescription = Nothing
    , _riName = pName_
    }

-- | The type of virtualization.
--
-- Default: 'paravirtual'
riVirtualizationType :: Lens' RegisterImage (Maybe Text)
riVirtualizationType = lens _riVirtualizationType (\ s a -> s{_riVirtualizationType = a});

-- | The full path to your AMI manifest in Amazon S3 storage.
riImageLocation :: Lens' RegisterImage (Maybe Text)
riImageLocation = lens _riImageLocation (\ s a -> s{_riImageLocation = a});

-- | The ID of the RAM disk.
riRAMDiskId :: Lens' RegisterImage (Maybe Text)
riRAMDiskId = lens _riRAMDiskId (\ s a -> s{_riRAMDiskId = a});

-- | The ID of the kernel.
riKernelId :: Lens' RegisterImage (Maybe Text)
riKernelId = lens _riKernelId (\ s a -> s{_riKernelId = a});

-- | The name of the root device (for example, '\/dev\/sda1', or
-- '\/dev\/xvda').
riRootDeviceName :: Lens' RegisterImage (Maybe Text)
riRootDeviceName = lens _riRootDeviceName (\ s a -> s{_riRootDeviceName = a});

-- | Set to 'simple' to enable enhanced networking for the AMI and any
-- instances that you launch from the AMI.
--
-- There is no way to disable enhanced networking at this time.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
riSRIOVNetSupport :: Lens' RegisterImage (Maybe Text)
riSRIOVNetSupport = lens _riSRIOVNetSupport (\ s a -> s{_riSRIOVNetSupport = a});

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, 'i386'. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
riArchitecture :: Lens' RegisterImage (Maybe ArchitectureValues)
riArchitecture = lens _riArchitecture (\ s a -> s{_riArchitecture = a});

-- | One or more block device mapping entries.
riBlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
riBlockDeviceMappings = lens _riBlockDeviceMappings (\ s a -> s{_riBlockDeviceMappings = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
riDryRun :: Lens' RegisterImage (Maybe Bool)
riDryRun = lens _riDryRun (\ s a -> s{_riDryRun = a});

-- | A description for your AMI.
riDescription :: Lens' RegisterImage (Maybe Text)
riDescription = lens _riDescription (\ s a -> s{_riDescription = a});

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\'), or underscores(_)
riName :: Lens' RegisterImage Text
riName = lens _riName (\ s a -> s{_riName = a});

instance AWSRequest RegisterImage where
        type Rs RegisterImage = RegisterImageResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 RegisterImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance ToHeaders RegisterImage where
        toHeaders = const mempty

instance ToPath RegisterImage where
        toPath = const "/"

instance ToQuery RegisterImage where
        toQuery RegisterImage'{..}
          = mconcat
              ["Action" =: ("RegisterImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VirtualizationType" =: _riVirtualizationType,
               "ImageLocation" =: _riImageLocation,
               "RamdiskId" =: _riRAMDiskId,
               "KernelId" =: _riKernelId,
               "RootDeviceName" =: _riRootDeviceName,
               "SriovNetSupport" =: _riSRIOVNetSupport,
               "Architecture" =: _riArchitecture,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _riBlockDeviceMappings),
               "DryRun" =: _riDryRun,
               "Description" =: _riDescription, "Name" =: _riName]

-- | /See:/ 'registerImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
    { _rirsImageId :: !(Maybe Text)
    , _rirsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsImageId'
--
-- * 'rirsStatus'
registerImageResponse
    :: Int -- ^ 'rirsStatus'
    -> RegisterImageResponse
registerImageResponse pStatus_ =
    RegisterImageResponse'
    { _rirsImageId = Nothing
    , _rirsStatus = pStatus_
    }

-- | The ID of the newly registered AMI.
rirsImageId :: Lens' RegisterImageResponse (Maybe Text)
rirsImageId = lens _rirsImageId (\ s a -> s{_rirsImageId = a});

-- | The response status code.
rirsStatus :: Lens' RegisterImageResponse Int
rirsStatus = lens _rirsStatus (\ s a -> s{_rirsStatus = a});
