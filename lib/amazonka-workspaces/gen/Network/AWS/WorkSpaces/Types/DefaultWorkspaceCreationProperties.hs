{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the default values that are used to create WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
--
--
-- /See:/ 'defaultWorkspaceCreationProperties' smart constructor.
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'
  { _dwcpCustomSecurityGroupId ::
      !(Maybe Text),
    _dwcpUserEnabledAsLocalAdministrator ::
      !(Maybe Bool),
    _dwcpEnableWorkDocs ::
      !(Maybe Bool),
    _dwcpEnableMaintenanceMode ::
      !(Maybe Bool),
    _dwcpEnableInternetAccess ::
      !(Maybe Bool),
    _dwcpDefaultOu ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultWorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwcpCustomSecurityGroupId' - The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
--
-- * 'dwcpUserEnabledAsLocalAdministrator' - Specifies whether WorkSpace users are local administrators on their WorkSpaces.
--
-- * 'dwcpEnableWorkDocs' - Specifies whether the directory is enabled for Amazon WorkDocs.
--
-- * 'dwcpEnableMaintenanceMode' - Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- * 'dwcpEnableInternetAccess' - Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
--
-- * 'dwcpDefaultOu' - The organizational unit (OU) in the directory for the WorkSpace machine accounts.
defaultWorkspaceCreationProperties ::
  DefaultWorkspaceCreationProperties
defaultWorkspaceCreationProperties =
  DefaultWorkspaceCreationProperties'
    { _dwcpCustomSecurityGroupId =
        Nothing,
      _dwcpUserEnabledAsLocalAdministrator = Nothing,
      _dwcpEnableWorkDocs = Nothing,
      _dwcpEnableMaintenanceMode = Nothing,
      _dwcpEnableInternetAccess = Nothing,
      _dwcpDefaultOu = Nothing
    }

-- | The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
dwcpCustomSecurityGroupId :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpCustomSecurityGroupId = lens _dwcpCustomSecurityGroupId (\s a -> s {_dwcpCustomSecurityGroupId = a})

-- | Specifies whether WorkSpace users are local administrators on their WorkSpaces.
dwcpUserEnabledAsLocalAdministrator :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpUserEnabledAsLocalAdministrator = lens _dwcpUserEnabledAsLocalAdministrator (\s a -> s {_dwcpUserEnabledAsLocalAdministrator = a})

-- | Specifies whether the directory is enabled for Amazon WorkDocs.
dwcpEnableWorkDocs :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableWorkDocs = lens _dwcpEnableWorkDocs (\s a -> s {_dwcpEnableWorkDocs = a})

-- | Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
dwcpEnableMaintenanceMode :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableMaintenanceMode = lens _dwcpEnableMaintenanceMode (\s a -> s {_dwcpEnableMaintenanceMode = a})

-- | Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
dwcpEnableInternetAccess :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableInternetAccess = lens _dwcpEnableInternetAccess (\s a -> s {_dwcpEnableInternetAccess = a})

-- | The organizational unit (OU) in the directory for the WorkSpace machine accounts.
dwcpDefaultOu :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpDefaultOu = lens _dwcpDefaultOu (\s a -> s {_dwcpDefaultOu = a})

instance FromJSON DefaultWorkspaceCreationProperties where
  parseJSON =
    withObject
      "DefaultWorkspaceCreationProperties"
      ( \x ->
          DefaultWorkspaceCreationProperties'
            <$> (x .:? "CustomSecurityGroupId")
            <*> (x .:? "UserEnabledAsLocalAdministrator")
            <*> (x .:? "EnableWorkDocs")
            <*> (x .:? "EnableMaintenanceMode")
            <*> (x .:? "EnableInternetAccess")
            <*> (x .:? "DefaultOu")
      )

instance Hashable DefaultWorkspaceCreationProperties

instance NFData DefaultWorkspaceCreationProperties
