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
-- Module      : Amazonka.EC2.Types.BaselineEbsBandwidthMbpsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BaselineEbsBandwidthMbpsRequest where

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
-- /See:/ 'newBaselineEbsBandwidthMbpsRequest' smart constructor.
data BaselineEbsBandwidthMbpsRequest = BaselineEbsBandwidthMbpsRequest'
  { -- | The maximum baseline bandwidth, in Mbps. To specify no maximum limit,
    -- omit this parameter.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum baseline bandwidth, in Mbps. To specify no minimum limit,
    -- omit this parameter.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaselineEbsBandwidthMbpsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'baselineEbsBandwidthMbpsRequest_max' - The maximum baseline bandwidth, in Mbps. To specify no maximum limit,
-- omit this parameter.
--
-- 'min', 'baselineEbsBandwidthMbpsRequest_min' - The minimum baseline bandwidth, in Mbps. To specify no minimum limit,
-- omit this parameter.
newBaselineEbsBandwidthMbpsRequest ::
  BaselineEbsBandwidthMbpsRequest
newBaselineEbsBandwidthMbpsRequest =
  BaselineEbsBandwidthMbpsRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum baseline bandwidth, in Mbps. To specify no maximum limit,
-- omit this parameter.
baselineEbsBandwidthMbpsRequest_max :: Lens.Lens' BaselineEbsBandwidthMbpsRequest (Prelude.Maybe Prelude.Int)
baselineEbsBandwidthMbpsRequest_max = Lens.lens (\BaselineEbsBandwidthMbpsRequest' {max} -> max) (\s@BaselineEbsBandwidthMbpsRequest' {} a -> s {max = a} :: BaselineEbsBandwidthMbpsRequest)

-- | The minimum baseline bandwidth, in Mbps. To specify no minimum limit,
-- omit this parameter.
baselineEbsBandwidthMbpsRequest_min :: Lens.Lens' BaselineEbsBandwidthMbpsRequest (Prelude.Maybe Prelude.Int)
baselineEbsBandwidthMbpsRequest_min = Lens.lens (\BaselineEbsBandwidthMbpsRequest' {min} -> min) (\s@BaselineEbsBandwidthMbpsRequest' {} a -> s {min = a} :: BaselineEbsBandwidthMbpsRequest)

instance
  Prelude.Hashable
    BaselineEbsBandwidthMbpsRequest
  where
  hashWithSalt
    _salt
    BaselineEbsBandwidthMbpsRequest' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    BaselineEbsBandwidthMbpsRequest
  where
  rnf BaselineEbsBandwidthMbpsRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery BaselineEbsBandwidthMbpsRequest where
  toQuery BaselineEbsBandwidthMbpsRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
