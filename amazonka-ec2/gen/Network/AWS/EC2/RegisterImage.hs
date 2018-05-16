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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AMI. When you're creating an AMI, this is the final step you must complete before you can launch an instance from the AMI. For more information about creating AMIs, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html Creating Your Own AMIs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- You can also use @RegisterImage@ to create an Amazon EBS-backed Linux AMI from a snapshot of a root device volume. You specify the snapshot using the block device mapping. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-launch-snapshot.html Launching a Linux Instance from a Backup> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can't register an image where a secondary (non-root) snapshot has AWS Marketplace product codes.
--
-- Some Linux distributions, such as Red Hat Enterprise Linux (RHEL) and SUSE Linux Enterprise Server (SLES), use the EC2 billing product code associated with an AMI to verify the subscription status for package updates. Creating an AMI from an EBS snapshot does not maintain this billing code, and subsequent instances launched from such an AMI will not be able to connect to package update infrastructure. To create an AMI that must retain billing codes, see 'CreateImage' .
--
-- If needed, you can deregister an AMI at any time. Any modifications you make to an AMI backed by an instance store volume invalidates its registration. If you make changes to an image, deregister the previous image and register the new image.
--
module Network.AWS.EC2.RegisterImage
    (
    -- * Creating a Request
      registerImage
    , RegisterImage
    -- * Request Lenses
    , riVirtualizationType
    , riImageLocation
    , riEnaSupport
    , riBillingProducts
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

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RegisterImage.
--
--
--
-- /See:/ 'registerImage' smart constructor.
data RegisterImage = RegisterImage'
  { _riVirtualizationType  :: !(Maybe Text)
  , _riImageLocation       :: !(Maybe Text)
  , _riEnaSupport          :: !(Maybe Bool)
  , _riBillingProducts     :: !(Maybe [Text])
  , _riRAMDiskId           :: !(Maybe Text)
  , _riKernelId            :: !(Maybe Text)
  , _riRootDeviceName      :: !(Maybe Text)
  , _riSRIOVNetSupport     :: !(Maybe Text)
  , _riArchitecture        :: !(Maybe ArchitectureValues)
  , _riDescription         :: !(Maybe Text)
  , _riBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _riDryRun              :: !(Maybe Bool)
  , _riName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riVirtualizationType' - The type of virtualization (@hvm@ | @paravirtual@ ). Default: @paravirtual@
--
-- * 'riImageLocation' - The full path to your AMI manifest in Amazon S3 storage.
--
-- * 'riEnaSupport' - Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI. This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- * 'riBillingProducts' - The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
--
-- * 'riRAMDiskId' - The ID of the RAM disk.
--
-- * 'riKernelId' - The ID of the kernel.
--
-- * 'riRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'riSRIOVNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI. There is no way to disable @sriovNetSupport@ at this time. This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- * 'riArchitecture' - The architecture of the AMI. Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
--
-- * 'riDescription' - A description for your AMI.
--
-- * 'riBlockDeviceMappings' - One or more block device mapping entries.
--
-- * 'riDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'riName' - A name for your AMI. Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
registerImage
    :: Text -- ^ 'riName'
    -> RegisterImage
registerImage pName_ =
  RegisterImage'
    { _riVirtualizationType = Nothing
    , _riImageLocation = Nothing
    , _riEnaSupport = Nothing
    , _riBillingProducts = Nothing
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


-- | The type of virtualization (@hvm@ | @paravirtual@ ). Default: @paravirtual@
riVirtualizationType :: Lens' RegisterImage (Maybe Text)
riVirtualizationType = lens _riVirtualizationType (\ s a -> s{_riVirtualizationType = a})

-- | The full path to your AMI manifest in Amazon S3 storage.
riImageLocation :: Lens' RegisterImage (Maybe Text)
riImageLocation = lens _riImageLocation (\ s a -> s{_riImageLocation = a})

-- | Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI. This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
riEnaSupport :: Lens' RegisterImage (Maybe Bool)
riEnaSupport = lens _riEnaSupport (\ s a -> s{_riEnaSupport = a})

-- | The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
riBillingProducts :: Lens' RegisterImage [Text]
riBillingProducts = lens _riBillingProducts (\ s a -> s{_riBillingProducts = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
riRAMDiskId :: Lens' RegisterImage (Maybe Text)
riRAMDiskId = lens _riRAMDiskId (\ s a -> s{_riRAMDiskId = a})

-- | The ID of the kernel.
riKernelId :: Lens' RegisterImage (Maybe Text)
riKernelId = lens _riKernelId (\ s a -> s{_riKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
riRootDeviceName :: Lens' RegisterImage (Maybe Text)
riRootDeviceName = lens _riRootDeviceName (\ s a -> s{_riRootDeviceName = a})

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI. There is no way to disable @sriovNetSupport@ at this time. This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
riSRIOVNetSupport :: Lens' RegisterImage (Maybe Text)
riSRIOVNetSupport = lens _riSRIOVNetSupport (\ s a -> s{_riSRIOVNetSupport = a})

-- | The architecture of the AMI. Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
riArchitecture :: Lens' RegisterImage (Maybe ArchitectureValues)
riArchitecture = lens _riArchitecture (\ s a -> s{_riArchitecture = a})

-- | A description for your AMI.
riDescription :: Lens' RegisterImage (Maybe Text)
riDescription = lens _riDescription (\ s a -> s{_riDescription = a})

-- | One or more block device mapping entries.
riBlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
riBlockDeviceMappings = lens _riBlockDeviceMappings (\ s a -> s{_riBlockDeviceMappings = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
riDryRun :: Lens' RegisterImage (Maybe Bool)
riDryRun = lens _riDryRun (\ s a -> s{_riDryRun = a})

-- | A name for your AMI. Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
riName :: Lens' RegisterImage Text
riName = lens _riName (\ s a -> s{_riName = a})

instance AWSRequest RegisterImage where
        type Rs RegisterImage = RegisterImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RegisterImageResponse' <$>
                   (x .@? "imageId") <*> (pure (fromEnum s)))

instance Hashable RegisterImage where

instance NFData RegisterImage where

instance ToHeaders RegisterImage where
        toHeaders = const mempty

instance ToPath RegisterImage where
        toPath = const "/"

instance ToQuery RegisterImage where
        toQuery RegisterImage'{..}
          = mconcat
              ["Action" =: ("RegisterImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "VirtualizationType" =: _riVirtualizationType,
               "ImageLocation" =: _riImageLocation,
               "EnaSupport" =: _riEnaSupport,
               toQuery
                 (toQueryList "BillingProduct" <$>
                    _riBillingProducts),
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

-- | Contains the output of RegisterImage.
--
--
--
-- /See:/ 'registerImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
  { _rirsImageId        :: !(Maybe Text)
  , _rirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsImageId' - The ID of the newly registered AMI.
--
-- * 'rirsResponseStatus' - -- | The response status code.
registerImageResponse
    :: Int -- ^ 'rirsResponseStatus'
    -> RegisterImageResponse
registerImageResponse pResponseStatus_ =
  RegisterImageResponse'
    {_rirsImageId = Nothing, _rirsResponseStatus = pResponseStatus_}


-- | The ID of the newly registered AMI.
rirsImageId :: Lens' RegisterImageResponse (Maybe Text)
rirsImageId = lens _rirsImageId (\ s a -> s{_rirsImageId = a})

-- | -- | The response status code.
rirsResponseStatus :: Lens' RegisterImageResponse Int
rirsResponseStatus = lens _rirsResponseStatus (\ s a -> s{_rirsResponseStatus = a})

instance NFData RegisterImageResponse where
