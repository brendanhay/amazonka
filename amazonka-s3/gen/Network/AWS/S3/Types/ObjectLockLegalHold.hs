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
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHold
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockLegalHold where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockLegalHoldStatus

-- | A Legal Hold configuration for an object.
--
-- /See:/ 'newObjectLockLegalHold' smart constructor.
data ObjectLockLegalHold = ObjectLockLegalHold'
  { -- | Indicates whether the specified object has a Legal Hold in place.
    status :: Prelude.Maybe ObjectLockLegalHoldStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectLockLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'objectLockLegalHold_status' - Indicates whether the specified object has a Legal Hold in place.
newObjectLockLegalHold ::
  ObjectLockLegalHold
newObjectLockLegalHold =
  ObjectLockLegalHold' {status = Prelude.Nothing}

-- | Indicates whether the specified object has a Legal Hold in place.
objectLockLegalHold_status :: Lens.Lens' ObjectLockLegalHold (Prelude.Maybe ObjectLockLegalHoldStatus)
objectLockLegalHold_status = Lens.lens (\ObjectLockLegalHold' {status} -> status) (\s@ObjectLockLegalHold' {} a -> s {status = a} :: ObjectLockLegalHold)

instance Prelude.FromXML ObjectLockLegalHold where
  parseXML x =
    ObjectLockLegalHold'
      Prelude.<$> (x Prelude..@? "Status")

instance Prelude.Hashable ObjectLockLegalHold

instance Prelude.NFData ObjectLockLegalHold

instance Prelude.ToXML ObjectLockLegalHold where
  toXML ObjectLockLegalHold' {..} =
    Prelude.mconcat ["Status" Prelude.@= status]
