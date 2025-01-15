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
-- Module      : Amazonka.SSMIncidents.Types.PagerDutyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.PagerDutyConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.PagerDutyIncidentConfiguration

-- | Details about the PagerDuty configuration for a response plan.
--
-- /See:/ 'newPagerDutyConfiguration' smart constructor.
data PagerDutyConfiguration = PagerDutyConfiguration'
  { -- | The name of the PagerDuty configuration.
    name :: Prelude.Text,
    -- | Details about the PagerDuty service associated with the configuration.
    pagerDutyIncidentConfiguration :: PagerDutyIncidentConfiguration,
    -- | The ID of the Amazon Web Services Secrets Manager secret that stores
    -- your PagerDuty key, either a General Access REST API Key or User Token
    -- REST API Key, and other user credentials.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PagerDutyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'pagerDutyConfiguration_name' - The name of the PagerDuty configuration.
--
-- 'pagerDutyIncidentConfiguration', 'pagerDutyConfiguration_pagerDutyIncidentConfiguration' - Details about the PagerDuty service associated with the configuration.
--
-- 'secretId', 'pagerDutyConfiguration_secretId' - The ID of the Amazon Web Services Secrets Manager secret that stores
-- your PagerDuty key, either a General Access REST API Key or User Token
-- REST API Key, and other user credentials.
newPagerDutyConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'pagerDutyIncidentConfiguration'
  PagerDutyIncidentConfiguration ->
  -- | 'secretId'
  Prelude.Text ->
  PagerDutyConfiguration
newPagerDutyConfiguration
  pName_
  pPagerDutyIncidentConfiguration_
  pSecretId_ =
    PagerDutyConfiguration'
      { name = pName_,
        pagerDutyIncidentConfiguration =
          pPagerDutyIncidentConfiguration_,
        secretId = pSecretId_
      }

-- | The name of the PagerDuty configuration.
pagerDutyConfiguration_name :: Lens.Lens' PagerDutyConfiguration Prelude.Text
pagerDutyConfiguration_name = Lens.lens (\PagerDutyConfiguration' {name} -> name) (\s@PagerDutyConfiguration' {} a -> s {name = a} :: PagerDutyConfiguration)

-- | Details about the PagerDuty service associated with the configuration.
pagerDutyConfiguration_pagerDutyIncidentConfiguration :: Lens.Lens' PagerDutyConfiguration PagerDutyIncidentConfiguration
pagerDutyConfiguration_pagerDutyIncidentConfiguration = Lens.lens (\PagerDutyConfiguration' {pagerDutyIncidentConfiguration} -> pagerDutyIncidentConfiguration) (\s@PagerDutyConfiguration' {} a -> s {pagerDutyIncidentConfiguration = a} :: PagerDutyConfiguration)

-- | The ID of the Amazon Web Services Secrets Manager secret that stores
-- your PagerDuty key, either a General Access REST API Key or User Token
-- REST API Key, and other user credentials.
pagerDutyConfiguration_secretId :: Lens.Lens' PagerDutyConfiguration Prelude.Text
pagerDutyConfiguration_secretId = Lens.lens (\PagerDutyConfiguration' {secretId} -> secretId) (\s@PagerDutyConfiguration' {} a -> s {secretId = a} :: PagerDutyConfiguration)

instance Data.FromJSON PagerDutyConfiguration where
  parseJSON =
    Data.withObject
      "PagerDutyConfiguration"
      ( \x ->
          PagerDutyConfiguration'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "pagerDutyIncidentConfiguration")
            Prelude.<*> (x Data..: "secretId")
      )

instance Prelude.Hashable PagerDutyConfiguration where
  hashWithSalt _salt PagerDutyConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pagerDutyIncidentConfiguration
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData PagerDutyConfiguration where
  rnf PagerDutyConfiguration' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf pagerDutyIncidentConfiguration `Prelude.seq`
        Prelude.rnf secretId

instance Data.ToJSON PagerDutyConfiguration where
  toJSON PagerDutyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "pagerDutyIncidentConfiguration"
                  Data..= pagerDutyIncidentConfiguration
              ),
            Prelude.Just ("secretId" Data..= secretId)
          ]
      )
