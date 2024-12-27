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
-- Module      : Amazonka.S3.Types.ObjectLockRetention
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectLockRetention where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ObjectLockRetentionMode

-- | A Retention configuration for an object.
--
-- /See:/ 'newObjectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { -- | Indicates the Retention mode for the specified object.
    mode :: Prelude.Maybe ObjectLockRetentionMode,
    -- | The date on which this Object Lock Retention will expire.
    retainUntilDate :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
objectLockRetention_retainUntilDate = Lens.lens (\ObjectLockRetention' {retainUntilDate} -> retainUntilDate) (\s@ObjectLockRetention' {} a -> s {retainUntilDate = a} :: ObjectLockRetention) Prelude.. Lens.mapping Data._Time

instance Data.FromXML ObjectLockRetention where
  parseXML x =
    ObjectLockRetention'
      Prelude.<$> (x Data..@? "Mode")
      Prelude.<*> (x Data..@? "RetainUntilDate")

instance Prelude.Hashable ObjectLockRetention where
  hashWithSalt _salt ObjectLockRetention' {..} =
    _salt
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` retainUntilDate

instance Prelude.NFData ObjectLockRetention where
  rnf ObjectLockRetention' {..} =
    Prelude.rnf mode `Prelude.seq`
      Prelude.rnf retainUntilDate

instance Data.ToXML ObjectLockRetention where
  toXML ObjectLockRetention' {..} =
    Prelude.mconcat
      [ "Mode" Data.@= mode,
        "RetainUntilDate" Data.@= retainUntilDate
      ]
