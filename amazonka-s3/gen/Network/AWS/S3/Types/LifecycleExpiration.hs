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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | Container for the expiration for the lifecycle of the object.
--
-- /See:/ 'newLifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { -- | Indicates the lifetime, in days, of the objects that are subject to the
    -- rule. The value must be a non-zero positive integer.
    days :: Core.Maybe Core.Int,
    -- | Indicates whether Amazon S3 will remove a delete marker with no
    -- noncurrent versions. If set to true, the delete marker will be expired;
    -- if set to false the policy takes no action. This cannot be specified
    -- with Days or Date in a Lifecycle Expiration Policy.
    expiredObjectDeleteMarker :: Core.Maybe Core.Bool,
    -- | Indicates at what date the object is to be moved or deleted. Should be
    -- in GMT ISO 8601 Format.
    date :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { days = Core.Nothing,
      expiredObjectDeleteMarker = Core.Nothing,
      date = Core.Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
lifecycleExpiration_days :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.Int)
lifecycleExpiration_days = Lens.lens (\LifecycleExpiration' {days} -> days) (\s@LifecycleExpiration' {} a -> s {days = a} :: LifecycleExpiration)

-- | Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
lifecycleExpiration_expiredObjectDeleteMarker :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.Bool)
lifecycleExpiration_expiredObjectDeleteMarker = Lens.lens (\LifecycleExpiration' {expiredObjectDeleteMarker} -> expiredObjectDeleteMarker) (\s@LifecycleExpiration' {} a -> s {expiredObjectDeleteMarker = a} :: LifecycleExpiration)

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
lifecycleExpiration_date :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.UTCTime)
lifecycleExpiration_date = Lens.lens (\LifecycleExpiration' {date} -> date) (\s@LifecycleExpiration' {} a -> s {date = a} :: LifecycleExpiration) Core.. Lens.mapping Core._Time

instance Core.FromXML LifecycleExpiration where
  parseXML x =
    LifecycleExpiration'
      Core.<$> (x Core..@? "Days")
      Core.<*> (x Core..@? "ExpiredObjectDeleteMarker")
      Core.<*> (x Core..@? "Date")

instance Core.Hashable LifecycleExpiration

instance Core.NFData LifecycleExpiration

instance Core.ToXML LifecycleExpiration where
  toXML LifecycleExpiration' {..} =
    Core.mconcat
      [ "Days" Core.@= days,
        "ExpiredObjectDeleteMarker"
          Core.@= expiredObjectDeleteMarker,
        "Date" Core.@= date
      ]
