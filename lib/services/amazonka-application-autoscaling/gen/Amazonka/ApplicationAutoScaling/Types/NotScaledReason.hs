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
-- Module      : Amazonka.ApplicationAutoScaling.Types.NotScaledReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.NotScaledReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the reason for an activity that isn\'t scaled (/not scaled
-- activity/), in machine-readable format. For help interpreting the not
-- scaled reason details, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scaling-activities.html Scaling activities for Application Auto Scaling>.
--
-- /See:/ 'newNotScaledReason' smart constructor.
data NotScaledReason = NotScaledReason'
  { -- | The current capacity.
    currentCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum capacity.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The minimum capacity.
    minCapacity :: Prelude.Maybe Prelude.Int,
    -- | A code that represents the reason for not scaling.
    --
    -- Valid values:
    --
    -- -   AutoScalingAnticipatedFlapping
    --
    -- -   TargetServicePutResourceAsUnscalable
    --
    -- -   AlreadyAtMaxCapacity
    --
    -- -   AlreadyAtMinCapacity
    --
    -- -   AlreadyAtDesiredCapacity
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotScaledReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentCapacity', 'notScaledReason_currentCapacity' - The current capacity.
--
-- 'maxCapacity', 'notScaledReason_maxCapacity' - The maximum capacity.
--
-- 'minCapacity', 'notScaledReason_minCapacity' - The minimum capacity.
--
-- 'code', 'notScaledReason_code' - A code that represents the reason for not scaling.
--
-- Valid values:
--
-- -   AutoScalingAnticipatedFlapping
--
-- -   TargetServicePutResourceAsUnscalable
--
-- -   AlreadyAtMaxCapacity
--
-- -   AlreadyAtMinCapacity
--
-- -   AlreadyAtDesiredCapacity
newNotScaledReason ::
  -- | 'code'
  Prelude.Text ->
  NotScaledReason
newNotScaledReason pCode_ =
  NotScaledReason'
    { currentCapacity = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      minCapacity = Prelude.Nothing,
      code = pCode_
    }

-- | The current capacity.
notScaledReason_currentCapacity :: Lens.Lens' NotScaledReason (Prelude.Maybe Prelude.Int)
notScaledReason_currentCapacity = Lens.lens (\NotScaledReason' {currentCapacity} -> currentCapacity) (\s@NotScaledReason' {} a -> s {currentCapacity = a} :: NotScaledReason)

-- | The maximum capacity.
notScaledReason_maxCapacity :: Lens.Lens' NotScaledReason (Prelude.Maybe Prelude.Int)
notScaledReason_maxCapacity = Lens.lens (\NotScaledReason' {maxCapacity} -> maxCapacity) (\s@NotScaledReason' {} a -> s {maxCapacity = a} :: NotScaledReason)

-- | The minimum capacity.
notScaledReason_minCapacity :: Lens.Lens' NotScaledReason (Prelude.Maybe Prelude.Int)
notScaledReason_minCapacity = Lens.lens (\NotScaledReason' {minCapacity} -> minCapacity) (\s@NotScaledReason' {} a -> s {minCapacity = a} :: NotScaledReason)

-- | A code that represents the reason for not scaling.
--
-- Valid values:
--
-- -   AutoScalingAnticipatedFlapping
--
-- -   TargetServicePutResourceAsUnscalable
--
-- -   AlreadyAtMaxCapacity
--
-- -   AlreadyAtMinCapacity
--
-- -   AlreadyAtDesiredCapacity
notScaledReason_code :: Lens.Lens' NotScaledReason Prelude.Text
notScaledReason_code = Lens.lens (\NotScaledReason' {code} -> code) (\s@NotScaledReason' {} a -> s {code = a} :: NotScaledReason)

instance Data.FromJSON NotScaledReason where
  parseJSON =
    Data.withObject
      "NotScaledReason"
      ( \x ->
          NotScaledReason'
            Prelude.<$> (x Data..:? "CurrentCapacity")
            Prelude.<*> (x Data..:? "MaxCapacity")
            Prelude.<*> (x Data..:? "MinCapacity")
            Prelude.<*> (x Data..: "Code")
      )

instance Prelude.Hashable NotScaledReason where
  hashWithSalt _salt NotScaledReason' {..} =
    _salt
      `Prelude.hashWithSalt` currentCapacity
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` minCapacity
      `Prelude.hashWithSalt` code

instance Prelude.NFData NotScaledReason where
  rnf NotScaledReason' {..} =
    Prelude.rnf currentCapacity
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf minCapacity
      `Prelude.seq` Prelude.rnf code
