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
-- Module      : Network.AWS.S3.Types.LifecycleExpiration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleExpiration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for the expiration for the lifecycle of the object.
--
-- /See:/ 'newLifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { -- | Indicates the lifetime, in days, of the objects that are subject to the
    -- rule. The value must be a non-zero positive integer.
    days :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether Amazon S3 will remove a delete marker with no
    -- noncurrent versions. If set to true, the delete marker will be expired;
    -- if set to false the policy takes no action. This cannot be specified
    -- with Days or Date in a Lifecycle Expiration Policy.
    expiredObjectDeleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | Indicates at what date the object is to be moved or deleted. Should be
    -- in GMT ISO 8601 Format.
    date :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LifecycleExpiration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'lifecycleExpiration_days' - Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
--
-- 'expiredObjectDeleteMarker', 'lifecycleExpiration_expiredObjectDeleteMarker' - Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
--
-- 'date', 'lifecycleExpiration_date' - Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
newLifecycleExpiration ::
  LifecycleExpiration
newLifecycleExpiration =
  LifecycleExpiration'
    { days = Prelude.Nothing,
      expiredObjectDeleteMarker = Prelude.Nothing,
      date = Prelude.Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
lifecycleExpiration_days :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.Int)
lifecycleExpiration_days = Lens.lens (\LifecycleExpiration' {days} -> days) (\s@LifecycleExpiration' {} a -> s {days = a} :: LifecycleExpiration)

-- | Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
lifecycleExpiration_expiredObjectDeleteMarker :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.Bool)
lifecycleExpiration_expiredObjectDeleteMarker = Lens.lens (\LifecycleExpiration' {expiredObjectDeleteMarker} -> expiredObjectDeleteMarker) (\s@LifecycleExpiration' {} a -> s {expiredObjectDeleteMarker = a} :: LifecycleExpiration)

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
lifecycleExpiration_date :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.UTCTime)
lifecycleExpiration_date = Lens.lens (\LifecycleExpiration' {date} -> date) (\s@LifecycleExpiration' {} a -> s {date = a} :: LifecycleExpiration) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML LifecycleExpiration where
  parseXML x =
    LifecycleExpiration'
      Prelude.<$> (x Prelude..@? "Days")
      Prelude.<*> (x Prelude..@? "ExpiredObjectDeleteMarker")
      Prelude.<*> (x Prelude..@? "Date")

instance Prelude.Hashable LifecycleExpiration

instance Prelude.NFData LifecycleExpiration

instance Prelude.ToXML LifecycleExpiration where
  toXML LifecycleExpiration' {..} =
    Prelude.mconcat
      [ "Days" Prelude.@= days,
        "ExpiredObjectDeleteMarker"
          Prelude.@= expiredObjectDeleteMarker,
        "Date" Prelude.@= date
      ]
