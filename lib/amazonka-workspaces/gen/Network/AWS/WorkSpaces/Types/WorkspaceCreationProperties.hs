{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the default properties that are used for creating WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
--
--
-- /See:/ 'workspaceCreationProperties' smart constructor.
data WorkspaceCreationProperties = WorkspaceCreationProperties'
  { _wcpCustomSecurityGroupId ::
      !(Maybe Text),
    _wcpUserEnabledAsLocalAdministrator ::
      !(Maybe Bool),
    _wcpEnableWorkDocs :: !(Maybe Bool),
    _wcpEnableMaintenanceMode ::
      !(Maybe Bool),
    _wcpEnableInternetAccess ::
      !(Maybe Bool),
    _wcpDefaultOu :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcpCustomSecurityGroupId' - The identifier of your custom security group.
--
-- * 'wcpUserEnabledAsLocalAdministrator' - Indicates whether users are local administrators of their WorkSpaces.
--
-- * 'wcpEnableWorkDocs' - Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
--
-- * 'wcpEnableMaintenanceMode' - Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- * 'wcpEnableInternetAccess' - Indicates whether internet access is enabled for your WorkSpaces.
--
-- * 'wcpDefaultOu' - The default organizational unit (OU) for your WorkSpaces directories. This string must be the full Lightweight Directory Access Protocol (LDAP) distinguished name for the target domain and OU. It must be in the form @"OU=/value/ ,DC=/value/ ,DC=/value/ "@ , where /value/ is any string of characters, and the number of domain components (DCs) is two or more. For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@ .  /Important:/     * To avoid errors, certain characters in the distinguished name must be escaped. For more information, see <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names> in the Microsoft documentation.     * The API doesn't validate whether the OU exists.
workspaceCreationProperties ::
  WorkspaceCreationProperties
workspaceCreationProperties =
  WorkspaceCreationProperties'
    { _wcpCustomSecurityGroupId = Nothing,
      _wcpUserEnabledAsLocalAdministrator = Nothing,
      _wcpEnableWorkDocs = Nothing,
      _wcpEnableMaintenanceMode = Nothing,
      _wcpEnableInternetAccess = Nothing,
      _wcpDefaultOu = Nothing
    }

-- | The identifier of your custom security group.
wcpCustomSecurityGroupId :: Lens' WorkspaceCreationProperties (Maybe Text)
wcpCustomSecurityGroupId = lens _wcpCustomSecurityGroupId (\s a -> s {_wcpCustomSecurityGroupId = a})

-- | Indicates whether users are local administrators of their WorkSpaces.
wcpUserEnabledAsLocalAdministrator :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpUserEnabledAsLocalAdministrator = lens _wcpUserEnabledAsLocalAdministrator (\s a -> s {_wcpUserEnabledAsLocalAdministrator = a})

-- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
wcpEnableWorkDocs :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpEnableWorkDocs = lens _wcpEnableWorkDocs (\s a -> s {_wcpEnableWorkDocs = a})

-- | Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
wcpEnableMaintenanceMode :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpEnableMaintenanceMode = lens _wcpEnableMaintenanceMode (\s a -> s {_wcpEnableMaintenanceMode = a})

-- | Indicates whether internet access is enabled for your WorkSpaces.
wcpEnableInternetAccess :: Lens' WorkspaceCreationProperties (Maybe Bool)
wcpEnableInternetAccess = lens _wcpEnableInternetAccess (\s a -> s {_wcpEnableInternetAccess = a})

-- | The default organizational unit (OU) for your WorkSpaces directories. This string must be the full Lightweight Directory Access Protocol (LDAP) distinguished name for the target domain and OU. It must be in the form @"OU=/value/ ,DC=/value/ ,DC=/value/ "@ , where /value/ is any string of characters, and the number of domain components (DCs) is two or more. For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@ .  /Important:/     * To avoid errors, certain characters in the distinguished name must be escaped. For more information, see <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names> in the Microsoft documentation.     * The API doesn't validate whether the OU exists.
wcpDefaultOu :: Lens' WorkspaceCreationProperties (Maybe Text)
wcpDefaultOu = lens _wcpDefaultOu (\s a -> s {_wcpDefaultOu = a})

instance Hashable WorkspaceCreationProperties

instance NFData WorkspaceCreationProperties

instance ToJSON WorkspaceCreationProperties where
  toJSON WorkspaceCreationProperties' {..} =
    object
      ( catMaybes
          [ ("CustomSecurityGroupId" .=) <$> _wcpCustomSecurityGroupId,
            ("UserEnabledAsLocalAdministrator" .=)
              <$> _wcpUserEnabledAsLocalAdministrator,
            ("EnableWorkDocs" .=) <$> _wcpEnableWorkDocs,
            ("EnableMaintenanceMode" .=) <$> _wcpEnableMaintenanceMode,
            ("EnableInternetAccess" .=) <$> _wcpEnableInternetAccess,
            ("DefaultOu" .=) <$> _wcpDefaultOu
          ]
      )
