{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.UsageResourceResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageResourceResult where

import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the sum of usage based on an AWS resource.
--
-- /See:/ 'newUsageResourceResult' smart constructor.
data UsageResourceResult = UsageResourceResult'
  { -- | Represents the sum total of usage for the specified resource type.
    total :: Prelude.Maybe Total,
    -- | The AWS resource that generated usage.
    resource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UsageResourceResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'usageResourceResult_total' - Represents the sum total of usage for the specified resource type.
--
-- 'resource', 'usageResourceResult_resource' - The AWS resource that generated usage.
newUsageResourceResult ::
  UsageResourceResult
newUsageResourceResult =
  UsageResourceResult'
    { total = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | Represents the sum total of usage for the specified resource type.
usageResourceResult_total :: Lens.Lens' UsageResourceResult (Prelude.Maybe Total)
usageResourceResult_total = Lens.lens (\UsageResourceResult' {total} -> total) (\s@UsageResourceResult' {} a -> s {total = a} :: UsageResourceResult)

-- | The AWS resource that generated usage.
usageResourceResult_resource :: Lens.Lens' UsageResourceResult (Prelude.Maybe Prelude.Text)
usageResourceResult_resource = Lens.lens (\UsageResourceResult' {resource} -> resource) (\s@UsageResourceResult' {} a -> s {resource = a} :: UsageResourceResult)

instance Prelude.FromJSON UsageResourceResult where
  parseJSON =
    Prelude.withObject
      "UsageResourceResult"
      ( \x ->
          UsageResourceResult'
            Prelude.<$> (x Prelude..:? "total")
            Prelude.<*> (x Prelude..:? "resource")
      )

instance Prelude.Hashable UsageResourceResult

instance Prelude.NFData UsageResourceResult
