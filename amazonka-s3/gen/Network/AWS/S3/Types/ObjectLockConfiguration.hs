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
-- Module      : Network.AWS.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockEnabled
import Network.AWS.S3.Types.ObjectLockRule

-- | The container element for Object Lock configuration parameters.
--
-- /See:/ 'newObjectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { -- | The Object Lock rule in place for the specified object.
    rule :: Core.Maybe ObjectLockRule,
    -- | Indicates whether this bucket has an Object Lock configuration enabled.
    objectLockEnabled :: Core.Maybe ObjectLockEnabled
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'objectLockConfiguration_rule' - The Object Lock rule in place for the specified object.
--
-- 'objectLockEnabled', 'objectLockConfiguration_objectLockEnabled' - Indicates whether this bucket has an Object Lock configuration enabled.
newObjectLockConfiguration ::
  ObjectLockConfiguration
newObjectLockConfiguration =
  ObjectLockConfiguration'
    { rule = Core.Nothing,
      objectLockEnabled = Core.Nothing
    }

-- | The Object Lock rule in place for the specified object.
objectLockConfiguration_rule :: Lens.Lens' ObjectLockConfiguration (Core.Maybe ObjectLockRule)
objectLockConfiguration_rule = Lens.lens (\ObjectLockConfiguration' {rule} -> rule) (\s@ObjectLockConfiguration' {} a -> s {rule = a} :: ObjectLockConfiguration)

-- | Indicates whether this bucket has an Object Lock configuration enabled.
objectLockConfiguration_objectLockEnabled :: Lens.Lens' ObjectLockConfiguration (Core.Maybe ObjectLockEnabled)
objectLockConfiguration_objectLockEnabled = Lens.lens (\ObjectLockConfiguration' {objectLockEnabled} -> objectLockEnabled) (\s@ObjectLockConfiguration' {} a -> s {objectLockEnabled = a} :: ObjectLockConfiguration)

instance Core.FromXML ObjectLockConfiguration where
  parseXML x =
    ObjectLockConfiguration'
      Core.<$> (x Core..@? "Rule")
      Core.<*> (x Core..@? "ObjectLockEnabled")

instance Core.Hashable ObjectLockConfiguration

instance Core.NFData ObjectLockConfiguration

instance Core.ToXML ObjectLockConfiguration where
  toXML ObjectLockConfiguration' {..} =
    Core.mconcat
      [ "Rule" Core.@= rule,
        "ObjectLockEnabled" Core.@= objectLockEnabled
      ]
