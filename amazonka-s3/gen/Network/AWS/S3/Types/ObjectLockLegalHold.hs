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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockLegalHoldStatus

-- | A Legal Hold configuration for an object.
--
-- /See:/ 'newObjectLockLegalHold' smart constructor.
data ObjectLockLegalHold = ObjectLockLegalHold'
  { -- | Indicates whether the specified object has a Legal Hold in place.
    status :: Core.Maybe ObjectLockLegalHoldStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ObjectLockLegalHold' {status = Core.Nothing}

-- | Indicates whether the specified object has a Legal Hold in place.
objectLockLegalHold_status :: Lens.Lens' ObjectLockLegalHold (Core.Maybe ObjectLockLegalHoldStatus)
objectLockLegalHold_status = Lens.lens (\ObjectLockLegalHold' {status} -> status) (\s@ObjectLockLegalHold' {} a -> s {status = a} :: ObjectLockLegalHold)

instance Core.FromXML ObjectLockLegalHold where
  parseXML x =
    ObjectLockLegalHold' Core.<$> (x Core..@? "Status")

instance Core.Hashable ObjectLockLegalHold

instance Core.NFData ObjectLockLegalHold

instance Core.ToXML ObjectLockLegalHold where
  toXML ObjectLockLegalHold' {..} =
    Core.mconcat ["Status" Core.@= status]
