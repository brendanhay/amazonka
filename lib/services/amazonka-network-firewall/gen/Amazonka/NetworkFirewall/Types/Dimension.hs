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
-- Module      : Amazonka.NetworkFirewall.Types.Dimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.Dimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value to use in an Amazon CloudWatch custom metric dimension. This
-- is used in the @PublishMetrics@ CustomAction. A CloudWatch custom metric
-- dimension is a name\/value pair that\'s part of the identity of a
-- metric.
--
-- Network Firewall sets the dimension name to @CustomAction@ and you
-- provide the dimension value.
--
-- For more information about CloudWatch custom metric dimensions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#usingDimensions Publishing Custom Metrics>
-- in the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
--
-- /See:/ 'newDimension' smart constructor.
data Dimension = Dimension'
  { -- | The value to use in the custom metric dimension.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'dimension_value' - The value to use in the custom metric dimension.
newDimension ::
  -- | 'value'
  Prelude.Text ->
  Dimension
newDimension pValue_ = Dimension' {value = pValue_}

-- | The value to use in the custom metric dimension.
dimension_value :: Lens.Lens' Dimension Prelude.Text
dimension_value = Lens.lens (\Dimension' {value} -> value) (\s@Dimension' {} a -> s {value = a} :: Dimension)

instance Data.FromJSON Dimension where
  parseJSON =
    Data.withObject
      "Dimension"
      (\x -> Dimension' Prelude.<$> (x Data..: "Value"))

instance Prelude.Hashable Dimension where
  hashWithSalt _salt Dimension' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData Dimension where
  rnf Dimension' {..} = Prelude.rnf value

instance Data.ToJSON Dimension where
  toJSON Dimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Value" Data..= value)]
      )
