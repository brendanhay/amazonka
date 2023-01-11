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
-- Module      : Amazonka.SecurityHub.Types.StatelessCustomPublishMetricActionDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StatelessCustomPublishMetricActionDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a CloudWatch dimension value to publish.
--
-- /See:/ 'newStatelessCustomPublishMetricActionDimension' smart constructor.
data StatelessCustomPublishMetricActionDimension = StatelessCustomPublishMetricActionDimension'
  { -- | The value to use for the custom metric dimension.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessCustomPublishMetricActionDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'statelessCustomPublishMetricActionDimension_value' - The value to use for the custom metric dimension.
newStatelessCustomPublishMetricActionDimension ::
  StatelessCustomPublishMetricActionDimension
newStatelessCustomPublishMetricActionDimension =
  StatelessCustomPublishMetricActionDimension'
    { value =
        Prelude.Nothing
    }

-- | The value to use for the custom metric dimension.
statelessCustomPublishMetricActionDimension_value :: Lens.Lens' StatelessCustomPublishMetricActionDimension (Prelude.Maybe Prelude.Text)
statelessCustomPublishMetricActionDimension_value = Lens.lens (\StatelessCustomPublishMetricActionDimension' {value} -> value) (\s@StatelessCustomPublishMetricActionDimension' {} a -> s {value = a} :: StatelessCustomPublishMetricActionDimension)

instance
  Data.FromJSON
    StatelessCustomPublishMetricActionDimension
  where
  parseJSON =
    Data.withObject
      "StatelessCustomPublishMetricActionDimension"
      ( \x ->
          StatelessCustomPublishMetricActionDimension'
            Prelude.<$> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    StatelessCustomPublishMetricActionDimension
  where
  hashWithSalt
    _salt
    StatelessCustomPublishMetricActionDimension' {..} =
      _salt `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    StatelessCustomPublishMetricActionDimension
  where
  rnf StatelessCustomPublishMetricActionDimension' {..} =
    Prelude.rnf value

instance
  Data.ToJSON
    StatelessCustomPublishMetricActionDimension
  where
  toJSON
    StatelessCustomPublishMetricActionDimension' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Value" Data..=) Prelude.<$> value]
        )
