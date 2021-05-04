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
-- Module      : Network.AWS.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockEnabled
import Network.AWS.S3.Types.ObjectLockRule

-- | The container element for Object Lock configuration parameters.
--
-- /See:/ 'newObjectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { -- | The Object Lock rule in place for the specified object.
    rule :: Prelude.Maybe ObjectLockRule,
    -- | Indicates whether this bucket has an Object Lock configuration enabled.
    objectLockEnabled :: Prelude.Maybe ObjectLockEnabled
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { rule = Prelude.Nothing,
      objectLockEnabled = Prelude.Nothing
    }

-- | The Object Lock rule in place for the specified object.
objectLockConfiguration_rule :: Lens.Lens' ObjectLockConfiguration (Prelude.Maybe ObjectLockRule)
objectLockConfiguration_rule = Lens.lens (\ObjectLockConfiguration' {rule} -> rule) (\s@ObjectLockConfiguration' {} a -> s {rule = a} :: ObjectLockConfiguration)

-- | Indicates whether this bucket has an Object Lock configuration enabled.
objectLockConfiguration_objectLockEnabled :: Lens.Lens' ObjectLockConfiguration (Prelude.Maybe ObjectLockEnabled)
objectLockConfiguration_objectLockEnabled = Lens.lens (\ObjectLockConfiguration' {objectLockEnabled} -> objectLockEnabled) (\s@ObjectLockConfiguration' {} a -> s {objectLockEnabled = a} :: ObjectLockConfiguration)

instance Prelude.FromXML ObjectLockConfiguration where
  parseXML x =
    ObjectLockConfiguration'
      Prelude.<$> (x Prelude..@? "Rule")
      Prelude.<*> (x Prelude..@? "ObjectLockEnabled")

instance Prelude.Hashable ObjectLockConfiguration

instance Prelude.NFData ObjectLockConfiguration

instance Prelude.ToXML ObjectLockConfiguration where
  toXML ObjectLockConfiguration' {..} =
    Prelude.mconcat
      [ "Rule" Prelude.@= rule,
        "ObjectLockEnabled" Prelude.@= objectLockEnabled
      ]
