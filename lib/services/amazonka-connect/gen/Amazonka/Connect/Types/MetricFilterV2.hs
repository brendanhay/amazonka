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
-- Module      : Amazonka.Connect.Types.MetricFilterV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.MetricFilterV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the filter used when retrieving metrics.
-- @MetricFiltersV2@ can be used on the following metrics:
-- @AVG_AGENT_CONNECTING_TIME@, @CONTACTS_CREATED@, @CONTACTS_HANDLED@,
-- @SUM_CONTACTS_DISCONNECTED@.
--
-- /See:/ 'newMetricFilterV2' smart constructor.
data MetricFilterV2 = MetricFilterV2'
  { -- | The key to use for filtering data.
    --
    -- Valid metric filter keys: @INITIATION_METHOD@, @DISCONNECT_REASON@.
    -- These are the same values as the @InitiationMethod@ and
    -- @DisconnectReason@ in the contact record. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/ctr-data-model.html#ctr-ContactTraceRecord ContactTraceRecord>
    -- in the /Amazon Connect Administrator\'s Guide/.
    metricFilterKey :: Prelude.Maybe Prelude.Text,
    -- | The values to use for filtering data.
    --
    -- Valid metric filter values for @INITIATION_METHOD@: @INBOUND@ |
    -- @OUTBOUND@ | @TRANSFER@ | @QUEUE_TRANSFER@ | @CALLBACK@ | @API@
    --
    -- Valid metric filter values for @DISCONNECT_REASON@:
    -- @CUSTOMER_DISCONNECT@ | @AGENT_DISCONNECT@ | @THIRD_PARTY_DISCONNECT@ |
    -- @TELECOM_PROBLEM@ | @BARGED@ | @CONTACT_FLOW_DISCONNECT@ | @OTHER@ |
    -- @EXPIRED@ | @API@
    metricFilterValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricFilterV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricFilterKey', 'metricFilterV2_metricFilterKey' - The key to use for filtering data.
--
-- Valid metric filter keys: @INITIATION_METHOD@, @DISCONNECT_REASON@.
-- These are the same values as the @InitiationMethod@ and
-- @DisconnectReason@ in the contact record. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/ctr-data-model.html#ctr-ContactTraceRecord ContactTraceRecord>
-- in the /Amazon Connect Administrator\'s Guide/.
--
-- 'metricFilterValues', 'metricFilterV2_metricFilterValues' - The values to use for filtering data.
--
-- Valid metric filter values for @INITIATION_METHOD@: @INBOUND@ |
-- @OUTBOUND@ | @TRANSFER@ | @QUEUE_TRANSFER@ | @CALLBACK@ | @API@
--
-- Valid metric filter values for @DISCONNECT_REASON@:
-- @CUSTOMER_DISCONNECT@ | @AGENT_DISCONNECT@ | @THIRD_PARTY_DISCONNECT@ |
-- @TELECOM_PROBLEM@ | @BARGED@ | @CONTACT_FLOW_DISCONNECT@ | @OTHER@ |
-- @EXPIRED@ | @API@
newMetricFilterV2 ::
  MetricFilterV2
newMetricFilterV2 =
  MetricFilterV2'
    { metricFilterKey = Prelude.Nothing,
      metricFilterValues = Prelude.Nothing
    }

-- | The key to use for filtering data.
--
-- Valid metric filter keys: @INITIATION_METHOD@, @DISCONNECT_REASON@.
-- These are the same values as the @InitiationMethod@ and
-- @DisconnectReason@ in the contact record. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/ctr-data-model.html#ctr-ContactTraceRecord ContactTraceRecord>
-- in the /Amazon Connect Administrator\'s Guide/.
metricFilterV2_metricFilterKey :: Lens.Lens' MetricFilterV2 (Prelude.Maybe Prelude.Text)
metricFilterV2_metricFilterKey = Lens.lens (\MetricFilterV2' {metricFilterKey} -> metricFilterKey) (\s@MetricFilterV2' {} a -> s {metricFilterKey = a} :: MetricFilterV2)

-- | The values to use for filtering data.
--
-- Valid metric filter values for @INITIATION_METHOD@: @INBOUND@ |
-- @OUTBOUND@ | @TRANSFER@ | @QUEUE_TRANSFER@ | @CALLBACK@ | @API@
--
-- Valid metric filter values for @DISCONNECT_REASON@:
-- @CUSTOMER_DISCONNECT@ | @AGENT_DISCONNECT@ | @THIRD_PARTY_DISCONNECT@ |
-- @TELECOM_PROBLEM@ | @BARGED@ | @CONTACT_FLOW_DISCONNECT@ | @OTHER@ |
-- @EXPIRED@ | @API@
metricFilterV2_metricFilterValues :: Lens.Lens' MetricFilterV2 (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
metricFilterV2_metricFilterValues = Lens.lens (\MetricFilterV2' {metricFilterValues} -> metricFilterValues) (\s@MetricFilterV2' {} a -> s {metricFilterValues = a} :: MetricFilterV2) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricFilterV2 where
  parseJSON =
    Data.withObject
      "MetricFilterV2"
      ( \x ->
          MetricFilterV2'
            Prelude.<$> (x Data..:? "MetricFilterKey")
            Prelude.<*> (x Data..:? "MetricFilterValues")
      )

instance Prelude.Hashable MetricFilterV2 where
  hashWithSalt _salt MetricFilterV2' {..} =
    _salt
      `Prelude.hashWithSalt` metricFilterKey
      `Prelude.hashWithSalt` metricFilterValues

instance Prelude.NFData MetricFilterV2 where
  rnf MetricFilterV2' {..} =
    Prelude.rnf metricFilterKey
      `Prelude.seq` Prelude.rnf metricFilterValues

instance Data.ToJSON MetricFilterV2 where
  toJSON MetricFilterV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricFilterKey" Data..=)
              Prelude.<$> metricFilterKey,
            ("MetricFilterValues" Data..=)
              Prelude.<$> metricFilterValues
          ]
      )
