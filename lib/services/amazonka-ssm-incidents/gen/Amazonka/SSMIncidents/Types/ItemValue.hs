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
-- Module      : Amazonka.SSMIncidents.Types.ItemValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ItemValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.PagerDutyIncidentDetail

-- | Describes a related item.
--
-- /See:/ 'newItemValue' smart constructor.
data ItemValue = ItemValue'
  { -- | The Amazon Resource Name (ARN) of the related item, if the related item
    -- is an Amazon resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The URL, if the related item is a non-Amazon Web Services resource.
    url :: Prelude.Maybe Prelude.Text,
    -- | The metric definition, if the related item is a metric in Amazon
    -- CloudWatch.
    metricDefinition :: Prelude.Maybe Prelude.Text,
    -- | Details about an incident that is associated with a PagerDuty incident.
    pagerDutyIncidentDetail :: Prelude.Maybe PagerDutyIncidentDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'itemValue_arn' - The Amazon Resource Name (ARN) of the related item, if the related item
-- is an Amazon resource.
--
-- 'url', 'itemValue_url' - The URL, if the related item is a non-Amazon Web Services resource.
--
-- 'metricDefinition', 'itemValue_metricDefinition' - The metric definition, if the related item is a metric in Amazon
-- CloudWatch.
--
-- 'pagerDutyIncidentDetail', 'itemValue_pagerDutyIncidentDetail' - Details about an incident that is associated with a PagerDuty incident.
newItemValue ::
  ItemValue
newItemValue =
  ItemValue'
    { arn = Prelude.Nothing,
      url = Prelude.Nothing,
      metricDefinition = Prelude.Nothing,
      pagerDutyIncidentDetail = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the related item, if the related item
-- is an Amazon resource.
itemValue_arn :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_arn = Lens.lens (\ItemValue' {arn} -> arn) (\s@ItemValue' {} a -> s {arn = a} :: ItemValue)

-- | The URL, if the related item is a non-Amazon Web Services resource.
itemValue_url :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_url = Lens.lens (\ItemValue' {url} -> url) (\s@ItemValue' {} a -> s {url = a} :: ItemValue)

-- | The metric definition, if the related item is a metric in Amazon
-- CloudWatch.
itemValue_metricDefinition :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_metricDefinition = Lens.lens (\ItemValue' {metricDefinition} -> metricDefinition) (\s@ItemValue' {} a -> s {metricDefinition = a} :: ItemValue)

-- | Details about an incident that is associated with a PagerDuty incident.
itemValue_pagerDutyIncidentDetail :: Lens.Lens' ItemValue (Prelude.Maybe PagerDutyIncidentDetail)
itemValue_pagerDutyIncidentDetail = Lens.lens (\ItemValue' {pagerDutyIncidentDetail} -> pagerDutyIncidentDetail) (\s@ItemValue' {} a -> s {pagerDutyIncidentDetail = a} :: ItemValue)

instance Data.FromJSON ItemValue where
  parseJSON =
    Data.withObject
      "ItemValue"
      ( \x ->
          ItemValue'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "metricDefinition")
            Prelude.<*> (x Data..:? "pagerDutyIncidentDetail")
      )

instance Prelude.Hashable ItemValue where
  hashWithSalt _salt ItemValue' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` metricDefinition
      `Prelude.hashWithSalt` pagerDutyIncidentDetail

instance Prelude.NFData ItemValue where
  rnf ItemValue' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf metricDefinition
      `Prelude.seq` Prelude.rnf pagerDutyIncidentDetail

instance Data.ToJSON ItemValue where
  toJSON ItemValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arn" Data..=) Prelude.<$> arn,
            ("url" Data..=) Prelude.<$> url,
            ("metricDefinition" Data..=)
              Prelude.<$> metricDefinition,
            ("pagerDutyIncidentDetail" Data..=)
              Prelude.<$> pagerDutyIncidentDetail
          ]
      )
