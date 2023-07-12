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
-- Module      : Amazonka.ImageBuilder.Types.LaunchPermissionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.LaunchPermissionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The ARN for an Amazon Web Services Organization that you want to share
    -- your AMI with. For more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What is Organizations?>.
    organizationArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN for an Organizations organizational unit (OU) that you want to
    -- share your AMI with. For more information about key concepts for
    -- Organizations, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html Organizations terminology and concepts>.
    organizationalUnitArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the group.
    userGroups :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account ID.
    userIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'organizationArns', 'launchPermissionConfiguration_organizationArns' - The ARN for an Amazon Web Services Organization that you want to share
-- your AMI with. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What is Organizations?>.
--
-- 'organizationalUnitArns', 'launchPermissionConfiguration_organizationalUnitArns' - The ARN for an Organizations organizational unit (OU) that you want to
-- share your AMI with. For more information about key concepts for
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html Organizations terminology and concepts>.
--
-- 'userGroups', 'launchPermissionConfiguration_userGroups' - The name of the group.
--
-- 'userIds', 'launchPermissionConfiguration_userIds' - The Amazon Web Services account ID.
newLaunchPermissionConfiguration ::
  LaunchPermissionConfiguration
newLaunchPermissionConfiguration =
  LaunchPermissionConfiguration'
    { organizationArns =
        Prelude.Nothing,
      organizationalUnitArns = Prelude.Nothing,
      userGroups = Prelude.Nothing,
      userIds = Prelude.Nothing
    }

-- | The ARN for an Amazon Web Services Organization that you want to share
-- your AMI with. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What is Organizations?>.
launchPermissionConfiguration_organizationArns :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchPermissionConfiguration_organizationArns = Lens.lens (\LaunchPermissionConfiguration' {organizationArns} -> organizationArns) (\s@LaunchPermissionConfiguration' {} a -> s {organizationArns = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for an Organizations organizational unit (OU) that you want to
-- share your AMI with. For more information about key concepts for
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html Organizations terminology and concepts>.
launchPermissionConfiguration_organizationalUnitArns :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchPermissionConfiguration_organizationalUnitArns = Lens.lens (\LaunchPermissionConfiguration' {organizationalUnitArns} -> organizationalUnitArns) (\s@LaunchPermissionConfiguration' {} a -> s {organizationalUnitArns = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the group.
launchPermissionConfiguration_userGroups :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe [Prelude.Text])
launchPermissionConfiguration_userGroups = Lens.lens (\LaunchPermissionConfiguration' {userGroups} -> userGroups) (\s@LaunchPermissionConfiguration' {} a -> s {userGroups = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
launchPermissionConfiguration_userIds :: Lens.Lens' LaunchPermissionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchPermissionConfiguration_userIds = Lens.lens (\LaunchPermissionConfiguration' {userIds} -> userIds) (\s@LaunchPermissionConfiguration' {} a -> s {userIds = a} :: LaunchPermissionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LaunchPermissionConfiguration where
  parseJSON =
    Data.withObject
      "LaunchPermissionConfiguration"
      ( \x ->
          LaunchPermissionConfiguration'
            Prelude.<$> (x Data..:? "organizationArns")
            Prelude.<*> (x Data..:? "organizationalUnitArns")
            Prelude.<*> (x Data..:? "userGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "userIds")
      )

instance
  Prelude.Hashable
    LaunchPermissionConfiguration
  where
  hashWithSalt _salt LaunchPermissionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` organizationArns
      `Prelude.hashWithSalt` organizationalUnitArns
      `Prelude.hashWithSalt` userGroups
      `Prelude.hashWithSalt` userIds

instance Prelude.NFData LaunchPermissionConfiguration where
  rnf LaunchPermissionConfiguration' {..} =
    Prelude.rnf organizationArns
      `Prelude.seq` Prelude.rnf organizationalUnitArns
      `Prelude.seq` Prelude.rnf userGroups
      `Prelude.seq` Prelude.rnf userIds

instance Data.ToJSON LaunchPermissionConfiguration where
  toJSON LaunchPermissionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("organizationArns" Data..=)
              Prelude.<$> organizationArns,
            ("organizationalUnitArns" Data..=)
              Prelude.<$> organizationalUnitArns,
            ("userGroups" Data..=) Prelude.<$> userGroups,
            ("userIds" Data..=) Prelude.<$> userIds
          ]
      )
