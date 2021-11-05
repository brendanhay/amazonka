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
-- Module      : Network.AWS.ImageBuilder.Types.LaunchTemplateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImageBuilder.Types.LaunchTemplateConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies an Amazon EC2 launch template to use for a specific account.
--
-- /See:/ 'newLaunchTemplateConfiguration' smart constructor.
data LaunchTemplateConfiguration = LaunchTemplateConfiguration'
  { -- | Set the specified Amazon EC2 launch template as the default launch
    -- template for the specified account.
    setDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The account ID that this configuration applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the Amazon EC2 launch template to use.
    launchTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setDefaultVersion', 'launchTemplateConfiguration_setDefaultVersion' - Set the specified Amazon EC2 launch template as the default launch
-- template for the specified account.
--
-- 'accountId', 'launchTemplateConfiguration_accountId' - The account ID that this configuration applies to.
--
-- 'launchTemplateId', 'launchTemplateConfiguration_launchTemplateId' - Identifies the Amazon EC2 launch template to use.
newLaunchTemplateConfiguration ::
  -- | 'launchTemplateId'
  Prelude.Text ->
  LaunchTemplateConfiguration
newLaunchTemplateConfiguration pLaunchTemplateId_ =
  LaunchTemplateConfiguration'
    { setDefaultVersion =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      launchTemplateId = pLaunchTemplateId_
    }

-- | Set the specified Amazon EC2 launch template as the default launch
-- template for the specified account.
launchTemplateConfiguration_setDefaultVersion :: Lens.Lens' LaunchTemplateConfiguration (Prelude.Maybe Prelude.Bool)
launchTemplateConfiguration_setDefaultVersion = Lens.lens (\LaunchTemplateConfiguration' {setDefaultVersion} -> setDefaultVersion) (\s@LaunchTemplateConfiguration' {} a -> s {setDefaultVersion = a} :: LaunchTemplateConfiguration)

-- | The account ID that this configuration applies to.
launchTemplateConfiguration_accountId :: Lens.Lens' LaunchTemplateConfiguration (Prelude.Maybe Prelude.Text)
launchTemplateConfiguration_accountId = Lens.lens (\LaunchTemplateConfiguration' {accountId} -> accountId) (\s@LaunchTemplateConfiguration' {} a -> s {accountId = a} :: LaunchTemplateConfiguration)

-- | Identifies the Amazon EC2 launch template to use.
launchTemplateConfiguration_launchTemplateId :: Lens.Lens' LaunchTemplateConfiguration Prelude.Text
launchTemplateConfiguration_launchTemplateId = Lens.lens (\LaunchTemplateConfiguration' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateConfiguration' {} a -> s {launchTemplateId = a} :: LaunchTemplateConfiguration)

instance Core.FromJSON LaunchTemplateConfiguration where
  parseJSON =
    Core.withObject
      "LaunchTemplateConfiguration"
      ( \x ->
          LaunchTemplateConfiguration'
            Prelude.<$> (x Core..:? "setDefaultVersion")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..: "launchTemplateId")
      )

instance Prelude.Hashable LaunchTemplateConfiguration

instance Prelude.NFData LaunchTemplateConfiguration

instance Core.ToJSON LaunchTemplateConfiguration where
  toJSON LaunchTemplateConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("setDefaultVersion" Core..=)
              Prelude.<$> setDefaultVersion,
            ("accountId" Core..=) Prelude.<$> accountId,
            Prelude.Just
              ("launchTemplateId" Core..= launchTemplateId)
          ]
      )
