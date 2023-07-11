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
-- Module      : Amazonka.Scheduler.Types.TargetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.TargetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a target.
--
-- /See:/ 'newTargetSummary' smart constructor.
data TargetSummary = TargetSummary'
  { -- | The Amazon Resource Name (ARN) of the target.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'targetSummary_arn' - The Amazon Resource Name (ARN) of the target.
newTargetSummary ::
  -- | 'arn'
  Prelude.Text ->
  TargetSummary
newTargetSummary pArn_ = TargetSummary' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the target.
targetSummary_arn :: Lens.Lens' TargetSummary Prelude.Text
targetSummary_arn = Lens.lens (\TargetSummary' {arn} -> arn) (\s@TargetSummary' {} a -> s {arn = a} :: TargetSummary)

instance Data.FromJSON TargetSummary where
  parseJSON =
    Data.withObject
      "TargetSummary"
      (\x -> TargetSummary' Prelude.<$> (x Data..: "Arn"))

instance Prelude.Hashable TargetSummary where
  hashWithSalt _salt TargetSummary' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData TargetSummary where
  rnf TargetSummary' {..} = Prelude.rnf arn
