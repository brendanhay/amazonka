{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- You can also use @RegisterImage@ to create an Amazon EBS-backed AMI from
-- a snapshot of a root device volume. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_LaunchingInstanceFromSnapshot.html Launching an Instance from a Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If needed, you can deregister an AMI at any time. Any modifications you
-- make to an AMI backed by an instance store volume invalidates its
-- registration. If you make changes to an image, deregister the previous
-- image and register the new image.
--
-- You can\'t register an image where a secondary (non-root) snapshot has
-- AWS Marketplace product codes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RegisterImage.html>
module Network.AWS.EC2.RegisterImage
    (
    -- * Request
      RegisterImage
    -- ** Request constructor
    , registerImage
    -- ** Request lenses
    , rrqVirtualizationType
    , rrqImageLocation
    , rrqRAMDiskId
    , rrqKernelId
    , rrqRootDeviceName
    , rrqSRIOVNetSupport
    , rrqArchitecture
    , rrqBlockDeviceMappings
    , rrqDryRun
    , rrqDescription
    , rrqName

    -- * Response
    , RegisterImageResponse
    -- ** Response constructor
    , registerImageResponse
    -- ** Response lenses
    , rirsImageId
    , rirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrqVirtualizationType'
--
-- * 'rrqImageLocation'
--
-- * 'rrqRAMDiskId'
--
-- * 'rrqKernelId'
--
-- * 'rrqRootDeviceName'
--
-- * 'rrqSRIOVNetSupport'
--
-- * 'rrqArchitecture'
--
-- * 'rrqBlockDeviceMappings'
--
-- * 'rrqDryRun'
--
-- * 'rrqDescription'
--
-- * 'rrqName'
data RegisterImage = RegisterImage'
    { _rrqVirtualizationType  :: !(Maybe Text)
    , _rrqImageLocation       :: !(Maybe Text)
    , _rrqRAMDiskId           :: !(Maybe Text)
    , _rrqKernelId            :: !(Maybe Text)
    , _rrqRootDeviceName      :: !(Maybe Text)
    , _rrqSRIOVNetSupport     :: !(Maybe Text)
    , _rrqArchitecture        :: !(Maybe ArchitectureValues)
    , _rrqBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
    , _rrqDryRun              :: !(Maybe Bool)
    , _rrqDescription         :: !(Maybe Text)
    , _rrqName                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterImage' smart constructor.
registerImage :: Text -> RegisterImage
registerImage pName_ =
    RegisterImage'
    { _rrqVirtualizationType = Nothing
    , _rrqImageLocation = Nothing
    , _rrqRAMDiskId = Nothing
    , _rrqKernelId = Nothing
    , _rrqRootDeviceName = Nothing
    , _rrqSRIOVNetSupport = Nothing
    , _rrqArchitecture = Nothing
    , _rrqBlockDeviceMappings = Nothing
    , _rrqDryRun = Nothing
    , _rrqDescription = Nothing
    , _rrqName = pName_
    }

-- | The type of virtualization.
--
-- Default: @paravirtual@
rrqVirtualizationType :: Lens' RegisterImage (Maybe Text)
rrqVirtualizationType = lens _rrqVirtualizationType (\ s a -> s{_rrqVirtualizationType = a});

-- | The full path to your AMI manifest in Amazon S3 storage.
rrqImageLocation :: Lens' RegisterImage (Maybe Text)
rrqImageLocation = lens _rrqImageLocation (\ s a -> s{_rrqImageLocation = a});

-- | The ID of the RAM disk.
rrqRAMDiskId :: Lens' RegisterImage (Maybe Text)
rrqRAMDiskId = lens _rrqRAMDiskId (\ s a -> s{_rrqRAMDiskId = a});

-- | The ID of the kernel.
rrqKernelId :: Lens' RegisterImage (Maybe Text)
rrqKernelId = lens _rrqKernelId (\ s a -> s{_rrqKernelId = a});

-- | The name of the root device (for example, @\/dev\/sda1@, or
-- @\/dev\/xvda@).
rrqRootDeviceName :: Lens' RegisterImage (Maybe Text)
rrqRootDeviceName = lens _rrqRootDeviceName (\ s a -> s{_rrqRootDeviceName = a});

-- | Set to @simple@ to enable enhanced networking for the AMI and any
-- instances that you launch from the AMI.
--
-- There is no way to disable enhanced networking at this time.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
rrqSRIOVNetSupport :: Lens' RegisterImage (Maybe Text)
rrqSRIOVNetSupport = lens _rrqSRIOVNetSupport (\ s a -> s{_rrqSRIOVNetSupport = a});

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
rrqArchitecture :: Lens' RegisterImage (Maybe ArchitectureValues)
rrqArchitecture = lens _rrqArchitecture (\ s a -> s{_rrqArchitecture = a});

-- | One or more block device mapping entries.
rrqBlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
rrqBlockDeviceMappings = lens _rrqBlockDeviceMappings (\ s a -> s{_rrqBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rrqDryRun :: Lens' RegisterImage (Maybe Bool)
rrqDryRun = lens _rrqDryRun (\ s a -> s{_rrqDryRun = a});

-- | A description for your AMI.
rrqDescription :: Lens' RegisterImage (Maybe Text)
rrqDescription = lens _rrqDescription (\ s a -> s{_rrqDescription = a});

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
rrqName :: Lens' RegisterImage Text
rrqName = lens _rrqName (\ s a -> s{_rrqName = a});

instance AWSRequest RegisterImage where
        type Sv RegisterImage = EC2
        type Rs RegisterImage = RegisterImageResponse
        request = post
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
               "VirtualizationType" =: _rrqVirtualizationType,
               "ImageLocation" =: _rrqImageLocation,
               "RamdiskId" =: _rrqRAMDiskId,
               "KernelId" =: _rrqKernelId,
               "RootDeviceName" =: _rrqRootDeviceName,
               "SriovNetSupport" =: _rrqSRIOVNetSupport,
               "Architecture" =: _rrqArchitecture,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rrqBlockDeviceMappings),
               "DryRun" =: _rrqDryRun,
               "Description" =: _rrqDescription, "Name" =: _rrqName]

-- | /See:/ 'registerImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirsImageId'
--
-- * 'rirsStatus'
data RegisterImageResponse = RegisterImageResponse'
    { _rirsImageId :: !(Maybe Text)
    , _rirsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterImageResponse' smart constructor.
registerImageResponse :: Int -> RegisterImageResponse
registerImageResponse pStatus_ =
    RegisterImageResponse'
    { _rirsImageId = Nothing
    , _rirsStatus = pStatus_
    }

-- | The ID of the newly registered AMI.
rirsImageId :: Lens' RegisterImageResponse (Maybe Text)
rirsImageId = lens _rirsImageId (\ s a -> s{_rirsImageId = a});

-- | FIXME: Undocumented member.
rirsStatus :: Lens' RegisterImageResponse Int
rirsStatus = lens _rirsStatus (\ s a -> s{_rirsStatus = a});
