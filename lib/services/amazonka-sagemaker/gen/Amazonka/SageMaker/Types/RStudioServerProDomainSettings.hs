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
-- Module      : Amazonka.SageMaker.Types.RStudioServerProDomainSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RStudioServerProDomainSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ResourceSpec

-- | A collection of settings that configure the @RStudioServerPro@
-- Domain-level app.
--
-- /See:/ 'newRStudioServerProDomainSettings' smart constructor.
data RStudioServerProDomainSettings = RStudioServerProDomainSettings'
  { defaultResourceSpec :: Prelude.Maybe ResourceSpec,
    -- | A URL pointing to an RStudio Connect server.
    rStudioConnectUrl :: Prelude.Maybe Prelude.Text,
    -- | A URL pointing to an RStudio Package Manager server.
    rStudioPackageManagerUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the execution role for the @RStudioServerPro@ Domain-level
    -- app.
    domainExecutionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RStudioServerProDomainSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'rStudioServerProDomainSettings_defaultResourceSpec' - Undocumented member.
--
-- 'rStudioConnectUrl', 'rStudioServerProDomainSettings_rStudioConnectUrl' - A URL pointing to an RStudio Connect server.
--
-- 'rStudioPackageManagerUrl', 'rStudioServerProDomainSettings_rStudioPackageManagerUrl' - A URL pointing to an RStudio Package Manager server.
--
-- 'domainExecutionRoleArn', 'rStudioServerProDomainSettings_domainExecutionRoleArn' - The ARN of the execution role for the @RStudioServerPro@ Domain-level
-- app.
newRStudioServerProDomainSettings ::
  -- | 'domainExecutionRoleArn'
  Prelude.Text ->
  RStudioServerProDomainSettings
newRStudioServerProDomainSettings
  pDomainExecutionRoleArn_ =
    RStudioServerProDomainSettings'
      { defaultResourceSpec =
          Prelude.Nothing,
        rStudioConnectUrl = Prelude.Nothing,
        rStudioPackageManagerUrl = Prelude.Nothing,
        domainExecutionRoleArn =
          pDomainExecutionRoleArn_
      }

-- | Undocumented member.
rStudioServerProDomainSettings_defaultResourceSpec :: Lens.Lens' RStudioServerProDomainSettings (Prelude.Maybe ResourceSpec)
rStudioServerProDomainSettings_defaultResourceSpec = Lens.lens (\RStudioServerProDomainSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@RStudioServerProDomainSettings' {} a -> s {defaultResourceSpec = a} :: RStudioServerProDomainSettings)

-- | A URL pointing to an RStudio Connect server.
rStudioServerProDomainSettings_rStudioConnectUrl :: Lens.Lens' RStudioServerProDomainSettings (Prelude.Maybe Prelude.Text)
rStudioServerProDomainSettings_rStudioConnectUrl = Lens.lens (\RStudioServerProDomainSettings' {rStudioConnectUrl} -> rStudioConnectUrl) (\s@RStudioServerProDomainSettings' {} a -> s {rStudioConnectUrl = a} :: RStudioServerProDomainSettings)

-- | A URL pointing to an RStudio Package Manager server.
rStudioServerProDomainSettings_rStudioPackageManagerUrl :: Lens.Lens' RStudioServerProDomainSettings (Prelude.Maybe Prelude.Text)
rStudioServerProDomainSettings_rStudioPackageManagerUrl = Lens.lens (\RStudioServerProDomainSettings' {rStudioPackageManagerUrl} -> rStudioPackageManagerUrl) (\s@RStudioServerProDomainSettings' {} a -> s {rStudioPackageManagerUrl = a} :: RStudioServerProDomainSettings)

-- | The ARN of the execution role for the @RStudioServerPro@ Domain-level
-- app.
rStudioServerProDomainSettings_domainExecutionRoleArn :: Lens.Lens' RStudioServerProDomainSettings Prelude.Text
rStudioServerProDomainSettings_domainExecutionRoleArn = Lens.lens (\RStudioServerProDomainSettings' {domainExecutionRoleArn} -> domainExecutionRoleArn) (\s@RStudioServerProDomainSettings' {} a -> s {domainExecutionRoleArn = a} :: RStudioServerProDomainSettings)

instance Data.FromJSON RStudioServerProDomainSettings where
  parseJSON =
    Data.withObject
      "RStudioServerProDomainSettings"
      ( \x ->
          RStudioServerProDomainSettings'
            Prelude.<$> (x Data..:? "DefaultResourceSpec")
            Prelude.<*> (x Data..:? "RStudioConnectUrl")
            Prelude.<*> (x Data..:? "RStudioPackageManagerUrl")
            Prelude.<*> (x Data..: "DomainExecutionRoleArn")
      )

instance
  Prelude.Hashable
    RStudioServerProDomainSettings
  where
  hashWithSalt
    _salt
    RStudioServerProDomainSettings' {..} =
      _salt `Prelude.hashWithSalt` defaultResourceSpec
        `Prelude.hashWithSalt` rStudioConnectUrl
        `Prelude.hashWithSalt` rStudioPackageManagerUrl
        `Prelude.hashWithSalt` domainExecutionRoleArn

instance
  Prelude.NFData
    RStudioServerProDomainSettings
  where
  rnf RStudioServerProDomainSettings' {..} =
    Prelude.rnf defaultResourceSpec
      `Prelude.seq` Prelude.rnf rStudioConnectUrl
      `Prelude.seq` Prelude.rnf rStudioPackageManagerUrl
      `Prelude.seq` Prelude.rnf domainExecutionRoleArn

instance Data.ToJSON RStudioServerProDomainSettings where
  toJSON RStudioServerProDomainSettings' {..} =
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
