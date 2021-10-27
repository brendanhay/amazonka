{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImageBuilder.Types.LaunchPermissionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImageBuilder.Types.LaunchPermissionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration for a launch permission. The launch
-- permission modification request is sent to the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyImageAttribute.html Amazon EC2 ModifyImageAttribute>
-- API on behalf of the user for each Region they have selected to
-- distribute the AMI. To make an AMI public, set the launch permission
-- authorized accounts to @all@. See the examples for making an AMI public
-- at
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyImageAttribute.html Amazon EC2 ModifyImageAttribute>.
--
-- /See:/ 'newLaunchPermissionConfiguration' smart constructor.
data LaunchPermissionConfiguration = LaunchPermissionConfiguration'
  { -- | The Amazon Web Services account ID.
    userIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the group.
    userGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchPermissionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIds', 'launchPermissionConfiguration_userIds' - The Amazon Web Services account ID.
--
-- 'userGroups', 'launchPermissionConfiguration_userGroups' - The name of the group.
newLaunchPermissionConfiguration ::
  LaunchPermissionConfiguration
newLaunchPermissionConfiguration =
  LaunchPermissionConfiguration'
    { userIds =
        Prelude.Nothing,
      userGroups = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
launchPermissionConfiguration_userIds :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchPermissionConfiguration_userIds = Lens.lens (\LaunchPermissionConfiguration' {userIds} -> userIds) (\s@LaunchPermissionConfiguration' {} a -> s {userIds = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the group.
launchPermissionConfiguration_userGroups :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe [Prelude.Text])
launchPermissionConfiguration_userGroups = Lens.lens (\LaunchPermissionConfiguration' {userGroups} -> userGroups) (\s@LaunchPermissionConfiguration' {} a -> s {userGroups = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LaunchPermissionConfiguration where
  parseJSON =
    Core.withObject
      "LaunchPermissionConfiguration"
      ( \x ->
          LaunchPermissionConfiguration'
            Prelude.<$> (x Core..:? "userIds")
            Prelude.<*> (x Core..:? "userGroups" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    LaunchPermissionConfiguration

instance Prelude.NFData LaunchPermissionConfiguration

instance Core.ToJSON LaunchPermissionConfiguration where
  toJSON LaunchPermissionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("userIds" Core..=) Prelude.<$> userIds,
            ("userGroups" Core..=) Prelude.<$> userGroups
          ]
      )
