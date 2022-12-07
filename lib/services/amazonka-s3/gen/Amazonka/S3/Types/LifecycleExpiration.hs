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
-- Module      : Amazonka.S3.Types.LifecycleExpiration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LifecycleExpiration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container for the expiration for the lifecycle of the object.
--
-- /See:/ 'newLifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { -- | Indicates whether Amazon S3 will remove a delete marker with no
    -- noncurrent versions. If set to true, the delete marker will be expired;
    -- if set to false the policy takes no action. This cannot be specified
    -- with Days or Date in a Lifecycle Expiration Policy.
    expiredObjectDeleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | Indicates at what date the object is to be moved or deleted. Should be
    -- in GMT ISO 8601 Format.
    date :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the lifetime, in days, of the objects that are subject to the
    -- rule. The value must be a non-zero positive integer.
    days :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleExpiration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiredObjectDeleteMarker', 'lifecycleExpiration_expiredObjectDeleteMarker' - Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
--
-- 'date', 'lifecycleExpiration_date' - Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
--
-- 'days', 'lifecycleExpiration_days' - Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
newLifecycleExpiration ::
  LifecycleExpiration
newLifecycleExpiration =
  LifecycleExpiration'
    { expiredObjectDeleteMarker =
        Prelude.Nothing,
      date = Prelude.Nothing,
      days = Prelude.Nothing
    }

-- | Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
lifecycleExpiration_expiredObjectDeleteMarker :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.Bool)
lifecycleExpiration_expiredObjectDeleteMarker = Lens.lens (\LifecycleExpiration' {expiredObjectDeleteMarker} -> expiredObjectDeleteMarker) (\s@LifecycleExpiration' {} a -> s {expiredObjectDeleteMarker = a} :: LifecycleExpiration)

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
lifecycleExpiration_date :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.UTCTime)
lifecycleExpiration_date = Lens.lens (\LifecycleExpiration' {date} -> date) (\s@LifecycleExpiration' {} a -> s {date = a} :: LifecycleExpiration) Prelude.. Lens.mapping Data._Time

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
lifecycleExpiration_days :: Lens.Lens' LifecycleExpiration (Prelude.Maybe Prelude.Int)
lifecycleExpiration_days = Lens.lens (\LifecycleExpiration' {days} -> days) (\s@LifecycleExpiration' {} a -> s {days = a} :: LifecycleExpiration)

instance Data.FromXML LifecycleExpiration where
  parseXML x =
    LifecycleExpiration'
      Prelude.<$> (x Data..@? "ExpiredObjectDeleteMarker")
      Prelude.<*> (x Data..@? "Date")
      Prelude.<*> (x Data..@? "Days")

instance Prelude.Hashable LifecycleExpiration where
  hashWithSalt _salt LifecycleExpiration' {..} =
    _salt
      `Prelude.hashWithSalt` expiredObjectDeleteMarker
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` days

instance Prelude.NFData LifecycleExpiration where
  rnf LifecycleExpiration' {..} =
    Prelude.rnf expiredObjectDeleteMarker
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf days

instance Data.ToXML LifecycleExpiration where
  toXML LifecycleExpiration' {..} =
    Prelude.mconcat
      [ "ExpiredObjectDeleteMarker"
          Data.@= expiredObjectDeleteMarker,
        "Date" Data.@= date,
        "Days" Data.@= days
      ]
