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
-- Module      : Network.AWS.DeviceFarm.Types.ProblemDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ProblemDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a problem detail.
--
-- /See:/ 'newProblemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { -- | The problem detail\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The problem detail\'s name.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProblemDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'problemDetail_arn' - The problem detail\'s ARN.
--
-- 'name', 'problemDetail_name' - The problem detail\'s name.
newProblemDetail ::
  ProblemDetail
newProblemDetail =
  ProblemDetail'
    { arn = Core.Nothing,
      name = Core.Nothing
    }

-- | The problem detail\'s ARN.
problemDetail_arn :: Lens.Lens' ProblemDetail (Core.Maybe Core.Text)
problemDetail_arn = Lens.lens (\ProblemDetail' {arn} -> arn) (\s@ProblemDetail' {} a -> s {arn = a} :: ProblemDetail)

-- | The problem detail\'s name.
problemDetail_name :: Lens.Lens' ProblemDetail (Core.Maybe Core.Text)
problemDetail_name = Lens.lens (\ProblemDetail' {name} -> name) (\s@ProblemDetail' {} a -> s {name = a} :: ProblemDetail)

instance Core.FromJSON ProblemDetail where
  parseJSON =
    Core.withObject
      "ProblemDetail"
      ( \x ->
          ProblemDetail'
            Core.<$> (x Core..:? "arn") Core.<*> (x Core..:? "name")
      )

instance Core.Hashable ProblemDetail

instance Core.NFData ProblemDetail
