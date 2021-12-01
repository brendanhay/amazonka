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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ItemValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a related item.
--
-- /See:/ 'newItemValue' smart constructor.
data ItemValue = ItemValue'
  { -- | The Amazon Resource Name (ARN) of the related item, if the related item
    -- is an Amazon resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The URL, if the related item is a non-AWS resource.
    url :: Prelude.Maybe Prelude.Text,
    -- | The metric definition, if the related item is a metric in CloudWatch.
    metricDefinition :: Prelude.Maybe Prelude.Text
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
-- 'url', 'itemValue_url' - The URL, if the related item is a non-AWS resource.
--
-- 'metricDefinition', 'itemValue_metricDefinition' - The metric definition, if the related item is a metric in CloudWatch.
newItemValue ::
  ItemValue
newItemValue =
  ItemValue'
    { arn = Prelude.Nothing,
      url = Prelude.Nothing,
      metricDefinition = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the related item, if the related item
-- is an Amazon resource.
itemValue_arn :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_arn = Lens.lens (\ItemValue' {arn} -> arn) (\s@ItemValue' {} a -> s {arn = a} :: ItemValue)

-- | The URL, if the related item is a non-AWS resource.
itemValue_url :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_url = Lens.lens (\ItemValue' {url} -> url) (\s@ItemValue' {} a -> s {url = a} :: ItemValue)

-- | The metric definition, if the related item is a metric in CloudWatch.
itemValue_metricDefinition :: Lens.Lens' ItemValue (Prelude.Maybe Prelude.Text)
itemValue_metricDefinition = Lens.lens (\ItemValue' {metricDefinition} -> metricDefinition) (\s@ItemValue' {} a -> s {metricDefinition = a} :: ItemValue)

instance Core.FromJSON ItemValue where
  parseJSON =
    Core.withObject
      "ItemValue"
      ( \x ->
          ItemValue'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "metricDefinition")
      )

instance Prelude.Hashable ItemValue where
  hashWithSalt salt' ItemValue' {..} =
    salt' `Prelude.hashWithSalt` metricDefinition
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ItemValue where
  rnf ItemValue' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf metricDefinition
      `Prelude.seq` Prelude.rnf url

instance Core.ToJSON ItemValue where
  toJSON ItemValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("arn" Core..=) Prelude.<$> arn,
            ("url" Core..=) Prelude.<$> url,
            ("metricDefinition" Core..=)
              Prelude.<$> metricDefinition
          ]
      )
