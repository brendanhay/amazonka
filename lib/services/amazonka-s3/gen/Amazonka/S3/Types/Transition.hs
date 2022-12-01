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
-- Module      : Amazonka.S3.Types.Transition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Transition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.TransitionStorageClass

-- | Specifies when an object transitions to a specified storage class. For
-- more information about Amazon S3 lifecycle configuration rules, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newTransition' smart constructor.
data Transition = Transition'
  { -- | Indicates when objects are transitioned to the specified storage class.
    -- The date value must be in ISO 8601 format. The time is always midnight
    -- UTC.
    date :: Prelude.Maybe Core.ISO8601,
    -- | Indicates the number of days after creation when objects are
    -- transitioned to the specified storage class. The value must be a
    -- positive integer.
    days :: Prelude.Maybe Prelude.Int,
    -- | The storage class to which you want the object to transition.
    storageClass :: Prelude.Maybe TransitionStorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Transition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'date', 'transition_date' - Indicates when objects are transitioned to the specified storage class.
-- The date value must be in ISO 8601 format. The time is always midnight
-- UTC.
--
-- 'days', 'transition_days' - Indicates the number of days after creation when objects are
-- transitioned to the specified storage class. The value must be a
-- positive integer.
--
-- 'storageClass', 'transition_storageClass' - The storage class to which you want the object to transition.
newTransition ::
  Transition
newTransition =
  Transition'
    { date = Prelude.Nothing,
      days = Prelude.Nothing,
      storageClass = Prelude.Nothing
    }

-- | Indicates when objects are transitioned to the specified storage class.
-- The date value must be in ISO 8601 format. The time is always midnight
-- UTC.
transition_date :: Lens.Lens' Transition (Prelude.Maybe Prelude.UTCTime)
transition_date = Lens.lens (\Transition' {date} -> date) (\s@Transition' {} a -> s {date = a} :: Transition) Prelude.. Lens.mapping Core._Time

-- | Indicates the number of days after creation when objects are
-- transitioned to the specified storage class. The value must be a
-- positive integer.
transition_days :: Lens.Lens' Transition (Prelude.Maybe Prelude.Int)
transition_days = Lens.lens (\Transition' {days} -> days) (\s@Transition' {} a -> s {days = a} :: Transition)

-- | The storage class to which you want the object to transition.
transition_storageClass :: Lens.Lens' Transition (Prelude.Maybe TransitionStorageClass)
transition_storageClass = Lens.lens (\Transition' {storageClass} -> storageClass) (\s@Transition' {} a -> s {storageClass = a} :: Transition)

instance Core.FromXML Transition where
  parseXML x =
    Transition'
      Prelude.<$> (x Core..@? "Date")
      Prelude.<*> (x Core..@? "Days")
      Prelude.<*> (x Core..@? "StorageClass")

instance Prelude.Hashable Transition where
  hashWithSalt _salt Transition' {..} =
    _salt `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` days
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData Transition where
  rnf Transition' {..} =
    Prelude.rnf date
      `Prelude.seq` Prelude.rnf days
      `Prelude.seq` Prelude.rnf storageClass

instance Core.ToXML Transition where
  toXML Transition' {..} =
    Prelude.mconcat
      [ "Date" Core.@= date,
        "Days" Core.@= days,
        "StorageClass" Core.@= storageClass
      ]
