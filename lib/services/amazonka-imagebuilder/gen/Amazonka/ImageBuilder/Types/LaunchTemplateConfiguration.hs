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
-- Module      : Amazonka.ImageBuilder.Types.LaunchTemplateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.LaunchTemplateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies an Amazon EC2 launch template to use for a specific account.
--
-- /See:/ 'newLaunchTemplateConfiguration' smart constructor.
data LaunchTemplateConfiguration = LaunchTemplateConfiguration'
  { -- | The account ID that this configuration applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Set the specified Amazon EC2 launch template as the default launch
    -- template for the specified account.
    setDefaultVersion :: Prelude.Maybe Prelude.Bool,
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
-- 'accountId', 'launchTemplateConfiguration_accountId' - The account ID that this configuration applies to.
--
-- 'setDefaultVersion', 'launchTemplateConfiguration_setDefaultVersion' - Set the specified Amazon EC2 launch template as the default launch
-- template for the specified account.
--
-- 'launchTemplateId', 'launchTemplateConfiguration_launchTemplateId' - Identifies the Amazon EC2 launch template to use.
newLaunchTemplateConfiguration ::
  -- | 'launchTemplateId'
  Prelude.Text ->
  LaunchTemplateConfiguration
newLaunchTemplateConfiguration pLaunchTemplateId_ =
  LaunchTemplateConfiguration'
    { accountId =
        Prelude.Nothing,
      setDefaultVersion = Prelude.Nothing,
      launchTemplateId = pLaunchTemplateId_
    }

-- | The account ID that this configuration applies to.
launchTemplateConfiguration_accountId :: Lens.Lens' LaunchTemplateConfiguration (Prelude.Maybe Prelude.Text)
launchTemplateConfiguration_accountId = Lens.lens (\LaunchTemplateConfiguration' {accountId} -> accountId) (\s@LaunchTemplateConfiguration' {} a -> s {accountId = a} :: LaunchTemplateConfiguration)

-- | Set the specified Amazon EC2 launch template as the default launch
-- template for the specified account.
launchTemplateConfiguration_setDefaultVersion :: Lens.Lens' LaunchTemplateConfiguration (Prelude.Maybe Prelude.Bool)
launchTemplateConfiguration_setDefaultVersion = Lens.lens (\LaunchTemplateConfiguration' {setDefaultVersion} -> setDefaultVersion) (\s@LaunchTemplateConfiguration' {} a -> s {setDefaultVersion = a} :: LaunchTemplateConfiguration)

-- | Identifies the Amazon EC2 launch template to use.
launchTemplateConfiguration_launchTemplateId :: Lens.Lens' LaunchTemplateConfiguration Prelude.Text
launchTemplateConfiguration_launchTemplateId = Lens.lens (\LaunchTemplateConfiguration' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateConfiguration' {} a -> s {launchTemplateId = a} :: LaunchTemplateConfiguration)

instance Data.FromJSON LaunchTemplateConfiguration where
  parseJSON =
    Data.withObject
      "LaunchTemplateConfiguration"
      ( \x ->
          LaunchTemplateConfiguration'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "setDefaultVersion")
            Prelude.<*> (x Data..: "launchTemplateId")
      )

instance Prelude.Hashable LaunchTemplateConfiguration where
  hashWithSalt _salt LaunchTemplateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` setDefaultVersion
      `Prelude.hashWithSalt` launchTemplateId

instance Prelude.NFData LaunchTemplateConfiguration where
  rnf LaunchTemplateConfiguration' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf setDefaultVersion
      `Prelude.seq` Prelude.rnf launchTemplateId

instance Data.ToJSON LaunchTemplateConfiguration where
  toJSON LaunchTemplateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountId" Data..=) Prelude.<$> accountId,
            ("setDefaultVersion" Data..=)
              Prelude.<$> setDefaultVersion,
            Prelude.Just
              ("launchTemplateId" Data..= launchTemplateId)
          ]
      )
