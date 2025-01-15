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
-- Module      : Amazonka.SageMaker.Types.RStudioServerProDomainSettingsForUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RStudioServerProDomainSettingsForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ResourceSpec

-- | A collection of settings that update the current configuration for the
-- @RStudioServerPro@ Domain-level app.
--
-- /See:/ 'newRStudioServerProDomainSettingsForUpdate' smart constructor.
data RStudioServerProDomainSettingsForUpdate = RStudioServerProDomainSettingsForUpdate'
  { defaultResourceSpec :: Prelude.Maybe ResourceSpec,
    -- | A URL pointing to an RStudio Connect server.
    rStudioConnectUrl :: Prelude.Maybe Prelude.Text,
    -- | A URL pointing to an RStudio Package Manager server.
    rStudioPackageManagerUrl :: Prelude.Maybe Prelude.Text,
    -- | The execution role for the @RStudioServerPro@ Domain-level app.
    domainExecutionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RStudioServerProDomainSettingsForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'rStudioServerProDomainSettingsForUpdate_defaultResourceSpec' - Undocumented member.
--
-- 'rStudioConnectUrl', 'rStudioServerProDomainSettingsForUpdate_rStudioConnectUrl' - A URL pointing to an RStudio Connect server.
--
-- 'rStudioPackageManagerUrl', 'rStudioServerProDomainSettingsForUpdate_rStudioPackageManagerUrl' - A URL pointing to an RStudio Package Manager server.
--
-- 'domainExecutionRoleArn', 'rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn' - The execution role for the @RStudioServerPro@ Domain-level app.
newRStudioServerProDomainSettingsForUpdate ::
  -- | 'domainExecutionRoleArn'
  Prelude.Text ->
  RStudioServerProDomainSettingsForUpdate
newRStudioServerProDomainSettingsForUpdate
  pDomainExecutionRoleArn_ =
    RStudioServerProDomainSettingsForUpdate'
      { defaultResourceSpec =
          Prelude.Nothing,
        rStudioConnectUrl =
          Prelude.Nothing,
        rStudioPackageManagerUrl =
          Prelude.Nothing,
        domainExecutionRoleArn =
          pDomainExecutionRoleArn_
      }

-- | Undocumented member.
rStudioServerProDomainSettingsForUpdate_defaultResourceSpec :: Lens.Lens' RStudioServerProDomainSettingsForUpdate (Prelude.Maybe ResourceSpec)
rStudioServerProDomainSettingsForUpdate_defaultResourceSpec = Lens.lens (\RStudioServerProDomainSettingsForUpdate' {defaultResourceSpec} -> defaultResourceSpec) (\s@RStudioServerProDomainSettingsForUpdate' {} a -> s {defaultResourceSpec = a} :: RStudioServerProDomainSettingsForUpdate)

-- | A URL pointing to an RStudio Connect server.
rStudioServerProDomainSettingsForUpdate_rStudioConnectUrl :: Lens.Lens' RStudioServerProDomainSettingsForUpdate (Prelude.Maybe Prelude.Text)
rStudioServerProDomainSettingsForUpdate_rStudioConnectUrl = Lens.lens (\RStudioServerProDomainSettingsForUpdate' {rStudioConnectUrl} -> rStudioConnectUrl) (\s@RStudioServerProDomainSettingsForUpdate' {} a -> s {rStudioConnectUrl = a} :: RStudioServerProDomainSettingsForUpdate)

-- | A URL pointing to an RStudio Package Manager server.
rStudioServerProDomainSettingsForUpdate_rStudioPackageManagerUrl :: Lens.Lens' RStudioServerProDomainSettingsForUpdate (Prelude.Maybe Prelude.Text)
rStudioServerProDomainSettingsForUpdate_rStudioPackageManagerUrl = Lens.lens (\RStudioServerProDomainSettingsForUpdate' {rStudioPackageManagerUrl} -> rStudioPackageManagerUrl) (\s@RStudioServerProDomainSettingsForUpdate' {} a -> s {rStudioPackageManagerUrl = a} :: RStudioServerProDomainSettingsForUpdate)

-- | The execution role for the @RStudioServerPro@ Domain-level app.
rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn :: Lens.Lens' RStudioServerProDomainSettingsForUpdate Prelude.Text
rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn = Lens.lens (\RStudioServerProDomainSettingsForUpdate' {domainExecutionRoleArn} -> domainExecutionRoleArn) (\s@RStudioServerProDomainSettingsForUpdate' {} a -> s {domainExecutionRoleArn = a} :: RStudioServerProDomainSettingsForUpdate)

instance
  Prelude.Hashable
    RStudioServerProDomainSettingsForUpdate
  where
  hashWithSalt
    _salt
    RStudioServerProDomainSettingsForUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` defaultResourceSpec
        `Prelude.hashWithSalt` rStudioConnectUrl
        `Prelude.hashWithSalt` rStudioPackageManagerUrl
        `Prelude.hashWithSalt` domainExecutionRoleArn

instance
  Prelude.NFData
    RStudioServerProDomainSettingsForUpdate
  where
  rnf RStudioServerProDomainSettingsForUpdate' {..} =
    Prelude.rnf defaultResourceSpec `Prelude.seq`
      Prelude.rnf rStudioConnectUrl `Prelude.seq`
        Prelude.rnf rStudioPackageManagerUrl `Prelude.seq`
          Prelude.rnf domainExecutionRoleArn

instance
  Data.ToJSON
    RStudioServerProDomainSettingsForUpdate
  where
  toJSON RStudioServerProDomainSettingsForUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultResourceSpec" Data..=)
              Prelude.<$> defaultResourceSpec,
            ("RStudioConnectUrl" Data..=)
              Prelude.<$> rStudioConnectUrl,
            ("RStudioPackageManagerUrl" Data..=)
              Prelude.<$> rStudioPackageManagerUrl,
            Prelude.Just
              ( "DomainExecutionRoleArn"
                  Data..= domainExecutionRoleArn
              )
          ]
      )
