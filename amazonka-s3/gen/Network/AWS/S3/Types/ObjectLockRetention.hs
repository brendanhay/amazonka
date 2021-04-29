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
-- Module      : Network.AWS.S3.Types.ObjectLockRetention
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRetention where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | A Retention configuration for an object.
--
-- /See:/ 'newObjectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { -- | Indicates the Retention mode for the specified object.
    mode :: Prelude.Maybe ObjectLockRetentionMode,
    -- | The date on which this Object Lock Retention will expire.
    retainUntilDate :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectLockRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'objectLockRetention_mode' - Indicates the Retention mode for the specified object.
--
-- 'retainUntilDate', 'objectLockRetention_retainUntilDate' - The date on which this Object Lock Retention will expire.
newObjectLockRetention ::
  ObjectLockRetention
newObjectLockRetention =
  ObjectLockRetention'
    { mode = Prelude.Nothing,
      retainUntilDate = Prelude.Nothing
    }

-- | Indicates the Retention mode for the specified object.
objectLockRetention_mode :: Lens.Lens' ObjectLockRetention (Prelude.Maybe ObjectLockRetentionMode)
objectLockRetention_mode = Lens.lens (\ObjectLockRetention' {mode} -> mode) (\s@ObjectLockRetention' {} a -> s {mode = a} :: ObjectLockRetention)

-- | The date on which this Object Lock Retention will expire.
objectLockRetention_retainUntilDate :: Lens.Lens' ObjectLockRetention (Prelude.Maybe Prelude.UTCTime)
objectLockRetention_retainUntilDate = Lens.lens (\ObjectLockRetention' {retainUntilDate} -> retainUntilDate) (\s@ObjectLockRetention' {} a -> s {retainUntilDate = a} :: ObjectLockRetention) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML ObjectLockRetention where
  parseXML x =
    ObjectLockRetention'
      Prelude.<$> (x Prelude..@? "Mode")
      Prelude.<*> (x Prelude..@? "RetainUntilDate")

instance Prelude.Hashable ObjectLockRetention

instance Prelude.NFData ObjectLockRetention

instance Prelude.ToXML ObjectLockRetention where
  toXML ObjectLockRetention' {..} =
    Prelude.mconcat
      [ "Mode" Prelude.@= mode,
        "RetainUntilDate" Prelude.@= retainUntilDate
      ]
