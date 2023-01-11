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
-- Module      : Amazonka.SSMIncidents.Types.PagerDutyIncidentDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.PagerDutyIncidentDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the PagerDuty incident associated with an incident created
-- by an Incident Manager response plan.
--
-- /See:/ 'newPagerDutyIncidentDetail' smart constructor.
data PagerDutyIncidentDetail = PagerDutyIncidentDetail'
  { -- | Indicates whether to resolve the PagerDuty incident when you resolve the
    -- associated Incident Manager incident.
    autoResolve :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Secrets Manager secret that stores
    -- your PagerDuty key, either a General Access REST API Key or User Token
    -- REST API Key, and other user credentials.
    secretId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the incident associated with the PagerDuty service for the
    -- response plan.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PagerDutyIncidentDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoResolve', 'pagerDutyIncidentDetail_autoResolve' - Indicates whether to resolve the PagerDuty incident when you resolve the
-- associated Incident Manager incident.
--
-- 'secretId', 'pagerDutyIncidentDetail_secretId' - The ID of the Amazon Web Services Secrets Manager secret that stores
-- your PagerDuty key, either a General Access REST API Key or User Token
-- REST API Key, and other user credentials.
--
-- 'id', 'pagerDutyIncidentDetail_id' - The ID of the incident associated with the PagerDuty service for the
-- response plan.
newPagerDutyIncidentDetail ::
  -- | 'id'
  Prelude.Text ->
  PagerDutyIncidentDetail
newPagerDutyIncidentDetail pId_ =
  PagerDutyIncidentDetail'
    { autoResolve =
        Prelude.Nothing,
      secretId = Prelude.Nothing,
      id = pId_
    }

-- | Indicates whether to resolve the PagerDuty incident when you resolve the
-- associated Incident Manager incident.
pagerDutyIncidentDetail_autoResolve :: Lens.Lens' PagerDutyIncidentDetail (Prelude.Maybe Prelude.Bool)
pagerDutyIncidentDetail_autoResolve = Lens.lens (\PagerDutyIncidentDetail' {autoResolve} -> autoResolve) (\s@PagerDutyIncidentDetail' {} a -> s {autoResolve = a} :: PagerDutyIncidentDetail)

-- | The ID of the Amazon Web Services Secrets Manager secret that stores
-- your PagerDuty key, either a General Access REST API Key or User Token
-- REST API Key, and other user credentials.
pagerDutyIncidentDetail_secretId :: Lens.Lens' PagerDutyIncidentDetail (Prelude.Maybe Prelude.Text)
pagerDutyIncidentDetail_secretId = Lens.lens (\PagerDutyIncidentDetail' {secretId} -> secretId) (\s@PagerDutyIncidentDetail' {} a -> s {secretId = a} :: PagerDutyIncidentDetail)

-- | The ID of the incident associated with the PagerDuty service for the
-- response plan.
pagerDutyIncidentDetail_id :: Lens.Lens' PagerDutyIncidentDetail Prelude.Text
pagerDutyIncidentDetail_id = Lens.lens (\PagerDutyIncidentDetail' {id} -> id) (\s@PagerDutyIncidentDetail' {} a -> s {id = a} :: PagerDutyIncidentDetail)

instance Data.FromJSON PagerDutyIncidentDetail where
  parseJSON =
    Data.withObject
      "PagerDutyIncidentDetail"
      ( \x ->
          PagerDutyIncidentDetail'
            Prelude.<$> (x Data..:? "autoResolve")
            Prelude.<*> (x Data..:? "secretId")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable PagerDutyIncidentDetail where
  hashWithSalt _salt PagerDutyIncidentDetail' {..} =
    _salt `Prelude.hashWithSalt` autoResolve
      `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` id

instance Prelude.NFData PagerDutyIncidentDetail where
  rnf PagerDutyIncidentDetail' {..} =
    Prelude.rnf autoResolve
      `Prelude.seq` Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON PagerDutyIncidentDetail where
  toJSON PagerDutyIncidentDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoResolve" Data..=) Prelude.<$> autoResolve,
            ("secretId" Data..=) Prelude.<$> secretId,
            Prelude.Just ("id" Data..= id)
          ]
      )
