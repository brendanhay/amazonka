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
-- Module      : Network.AWS.S3.Types.ObjectLockRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DefaultRetention

-- | The container element for an Object Lock rule.
--
-- /See:/ 'newObjectLockRule' smart constructor.
data ObjectLockRule = ObjectLockRule'
  { -- | The default retention period that you want to apply to new objects
    -- placed in the specified bucket.
    defaultRetention :: Core.Maybe DefaultRetention
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ObjectLockRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultRetention', 'objectLockRule_defaultRetention' - The default retention period that you want to apply to new objects
-- placed in the specified bucket.
newObjectLockRule ::
  ObjectLockRule
newObjectLockRule =
  ObjectLockRule' {defaultRetention = Core.Nothing}

-- | The default retention period that you want to apply to new objects
-- placed in the specified bucket.
objectLockRule_defaultRetention :: Lens.Lens' ObjectLockRule (Core.Maybe DefaultRetention)
objectLockRule_defaultRetention = Lens.lens (\ObjectLockRule' {defaultRetention} -> defaultRetention) (\s@ObjectLockRule' {} a -> s {defaultRetention = a} :: ObjectLockRule)

instance Core.FromXML ObjectLockRule where
  parseXML x =
    ObjectLockRule'
      Core.<$> (x Core..@? "DefaultRetention")

instance Core.Hashable ObjectLockRule

instance Core.NFData ObjectLockRule

instance Core.ToXML ObjectLockRule where
  toXML ObjectLockRule' {..} =
    Core.mconcat
      ["DefaultRetention" Core.@= defaultRetention]
