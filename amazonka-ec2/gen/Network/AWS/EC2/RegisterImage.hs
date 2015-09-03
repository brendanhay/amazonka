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
-- you must complete before you can launch an instance from the AMI. For
-- more information about creating AMIs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html Creating Your Own AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For Amazon EBS-backed instances, CreateImage creates and registers the
-- AMI in a single request, so you don\'t have to register the AMI
-- yourself.
--
-- You can also use 'RegisterImage' to create an Amazon EBS-backed Linux
-- AMI from a snapshot of a root device volume. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_LaunchingInstanceFromSnapshot.html Launching an Instance from a Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Some Linux distributions, such as Red Hat Enterprise Linux (RHEL) and
-- SUSE Linux Enterprise Server (SLES), use the EC2 'billingProduct' code
-- associated with an AMI to verify subscription status for package
-- updates. Creating an AMI from an EBS snapshot does not maintain this
-- billing code, and subsequent instances launched from such an AMI will
-- not be able to connect to package update infrastructure.
--
-- Similarly, although you can create a Windows AMI from a snapshot, you
-- can\'t successfully launch an instance from the AMI.
--
-- To create Windows AMIs or to create AMIs for Linux operating systems
-- that must retain AMI billing codes to work properly, see CreateImage.
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
    , riDescription
    , riBlockDeviceMappings
    , riDryRun
    , riName

    -- * Destructuring the Response
    , registerImageResponse
    , RegisterImageResponse
    -- * Response Lenses
    , rirsImageId
    , rirsResponseStatus
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
    , _riDescription         :: !(Maybe Text)
    , _riBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _riDryRun              :: !(Maybe Bool)
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
-- * 'riDescription'
--
-- * 'riBlockDeviceMappings'
--
-- * 'riDryRun'
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
    , _riDescription = Nothing
    , _riBlockDeviceMappings = Nothing
    , _riDryRun = Nothing
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

-- | A description for your AMI.
riDescription :: Lens' RegisterImage (Maybe Text)
riDescription = lens _riDescription (\ s a -> s{_riDescription = a});

-- | One or more block device mapping entries.
riBlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
riBlockDeviceMappings = lens _riBlockDeviceMappings (\ s a -> s{_riBlockDeviceMappings = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
riDryRun :: Lens' RegisterImage (Maybe Bool)
riDryRun = lens _riDryRun (\ s a -> s{_riDryRun = a});

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
               "Description" =: _riDescription,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _riBlockDeviceMappings),
               "DryRun" =: _riDryRun, "Name" =: _riName]

-- | /See:/ 'registerImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
    { _rirsImageId        :: !(Maybe Text)
    , _rirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsImageId'
--
-- * 'rirsResponseStatus'
registerImageResponse
    :: Int -- ^ 'rirsResponseStatus'
    -> RegisterImageResponse
registerImageResponse pResponseStatus_ =
    RegisterImageResponse'
    { _rirsImageId = Nothing
    , _rirsResponseStatus = pResponseStatus_
    }

-- | The ID of the newly registered AMI.
rirsImageId :: Lens' RegisterImageResponse (Maybe Text)
rirsImageId = lens _rirsImageId (\ s a -> s{_rirsImageId = a});

-- | The response status code.
rirsResponseStatus :: Lens' RegisterImageResponse Int
rirsResponseStatus = lens _rirsResponseStatus (\ s a -> s{_rirsResponseStatus = a});
