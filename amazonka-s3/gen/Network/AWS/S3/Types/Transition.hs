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
-- Module      : Network.AWS.S3.Types.Transition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Transition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Specifies when an object transitions to a specified storage class. For
-- more information about Amazon S3 lifecycle configuration rules, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- /See:/ 'newTransition' smart constructor.
data Transition = Transition'
  { -- | Indicates the number of days after creation when objects are
    -- transitioned to the specified storage class. The value must be a
    -- positive integer.
    days :: Prelude.Maybe Prelude.Int,
    -- | The storage class to which you want the object to transition.
    storageClass :: Prelude.Maybe TransitionStorageClass,
    -- | Indicates when objects are transitioned to the specified storage class.
    -- The date value must be in ISO 8601 format. The time is always midnight
    -- UTC.
    date :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Transition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'transition_days' - Indicates the number of days after creation when objects are
-- transitioned to the specified storage class. The value must be a
-- positive integer.
--
-- 'storageClass', 'transition_storageClass' - The storage class to which you want the object to transition.
--
-- 'date', 'transition_date' - Indicates when objects are transitioned to the specified storage class.
-- The date value must be in ISO 8601 format. The time is always midnight
-- UTC.
newTransition ::
  Transition
newTransition =
  Transition'
    { days = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      date = Prelude.Nothing
    }

-- | Indicates the number of days after creation when objects are
-- transitioned to the specified storage class. The value must be a
-- positive integer.
transition_days :: Lens.Lens' Transition (Prelude.Maybe Prelude.Int)
transition_days = Lens.lens (\Transition' {days} -> days) (\s@Transition' {} a -> s {days = a} :: Transition)

-- | The storage class to which you want the object to transition.
transition_storageClass :: Lens.Lens' Transition (Prelude.Maybe TransitionStorageClass)
transition_storageClass = Lens.lens (\Transition' {storageClass} -> storageClass) (\s@Transition' {} a -> s {storageClass = a} :: Transition)

-- | Indicates when objects are transitioned to the specified storage class.
-- The date value must be in ISO 8601 format. The time is always midnight
-- UTC.
transition_date :: Lens.Lens' Transition (Prelude.Maybe Prelude.UTCTime)
transition_date = Lens.lens (\Transition' {date} -> date) (\s@Transition' {} a -> s {date = a} :: Transition) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML Transition where
  parseXML x =
    Transition'
      Prelude.<$> (x Prelude..@? "Days")
      Prelude.<*> (x Prelude..@? "StorageClass")
      Prelude.<*> (x Prelude..@? "Date")

instance Prelude.Hashable Transition

instance Prelude.NFData Transition

instance Prelude.ToXML Transition where
  toXML Transition' {..} =
    Prelude.mconcat
      [ "Days" Prelude.@= days,
        "StorageClass" Prelude.@= storageClass,
        "Date" Prelude.@= date
      ]
