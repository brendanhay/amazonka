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
-- Module      : Amazonka.EC2.Types.BaselineEbsBandwidthMbps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BaselineEbsBandwidthMbps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newBaselineEbsBandwidthMbps' smart constructor.
data BaselineEbsBandwidthMbps = BaselineEbsBandwidthMbps'
  { -- | The maximum baseline bandwidth, in Mbps. If this parameter is not
    -- specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum baseline bandwidth, in Mbps. If this parameter is not
    -- specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaselineEbsBandwidthMbps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'baselineEbsBandwidthMbps_max' - The maximum baseline bandwidth, in Mbps. If this parameter is not
-- specified, there is no maximum limit.
--
-- 'min', 'baselineEbsBandwidthMbps_min' - The minimum baseline bandwidth, in Mbps. If this parameter is not
-- specified, there is no minimum limit.
newBaselineEbsBandwidthMbps ::
  BaselineEbsBandwidthMbps
newBaselineEbsBandwidthMbps =
  BaselineEbsBandwidthMbps'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum baseline bandwidth, in Mbps. If this parameter is not
-- specified, there is no maximum limit.
baselineEbsBandwidthMbps_max :: Lens.Lens' BaselineEbsBandwidthMbps (Prelude.Maybe Prelude.Int)
baselineEbsBandwidthMbps_max = Lens.lens (\BaselineEbsBandwidthMbps' {max} -> max) (\s@BaselineEbsBandwidthMbps' {} a -> s {max = a} :: BaselineEbsBandwidthMbps)

-- | The minimum baseline bandwidth, in Mbps. If this parameter is not
-- specified, there is no minimum limit.
baselineEbsBandwidthMbps_min :: Lens.Lens' BaselineEbsBandwidthMbps (Prelude.Maybe Prelude.Int)
baselineEbsBandwidthMbps_min = Lens.lens (\BaselineEbsBandwidthMbps' {min} -> min) (\s@BaselineEbsBandwidthMbps' {} a -> s {min = a} :: BaselineEbsBandwidthMbps)

instance Data.FromXML BaselineEbsBandwidthMbps where
  parseXML x =
    BaselineEbsBandwidthMbps'
      Prelude.<$> (x Data..@? "max")
      Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable BaselineEbsBandwidthMbps where
  hashWithSalt _salt BaselineEbsBandwidthMbps' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData BaselineEbsBandwidthMbps where
  rnf BaselineEbsBandwidthMbps' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery BaselineEbsBandwidthMbps where
  toQuery BaselineEbsBandwidthMbps' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
