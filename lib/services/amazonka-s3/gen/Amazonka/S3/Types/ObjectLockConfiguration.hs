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
-- Module      : Amazonka.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectLockConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ObjectLockEnabled
import Amazonka.S3.Types.ObjectLockRule

-- | The container element for Object Lock configuration parameters.
--
-- /See:/ 'newObjectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { -- | Indicates whether this bucket has an Object Lock configuration enabled.
    -- Enable @ObjectLockEnabled@ when you apply @ObjectLockConfiguration@ to a
    -- bucket.
    objectLockEnabled :: Prelude.Maybe ObjectLockEnabled,
    -- | Specifies the Object Lock rule for the specified object. Enable the this
    -- rule when you apply @ObjectLockConfiguration@ to a bucket. Bucket
    -- settings require both a mode and a period. The period can be either
    -- @Days@ or @Years@ but you must select one. You cannot specify @Days@ and
    -- @Years@ at the same time.
    rule :: Prelude.Maybe ObjectLockRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectLockEnabled', 'objectLockConfiguration_objectLockEnabled' - Indicates whether this bucket has an Object Lock configuration enabled.
-- Enable @ObjectLockEnabled@ when you apply @ObjectLockConfiguration@ to a
-- bucket.
--
-- 'rule', 'objectLockConfiguration_rule' - Specifies the Object Lock rule for the specified object. Enable the this
-- rule when you apply @ObjectLockConfiguration@ to a bucket. Bucket
-- settings require both a mode and a period. The period can be either
-- @Days@ or @Years@ but you must select one. You cannot specify @Days@ and
-- @Years@ at the same time.
newObjectLockConfiguration ::
  ObjectLockConfiguration
newObjectLockConfiguration =
  ObjectLockConfiguration'
    { objectLockEnabled =
        Prelude.Nothing,
      rule = Prelude.Nothing
    }

-- | Indicates whether this bucket has an Object Lock configuration enabled.
-- Enable @ObjectLockEnabled@ when you apply @ObjectLockConfiguration@ to a
-- bucket.
objectLockConfiguration_objectLockEnabled :: Lens.Lens' ObjectLockConfiguration (Prelude.Maybe ObjectLockEnabled)
objectLockConfiguration_objectLockEnabled = Lens.lens (\ObjectLockConfiguration' {objectLockEnabled} -> objectLockEnabled) (\s@ObjectLockConfiguration' {} a -> s {objectLockEnabled = a} :: ObjectLockConfiguration)

-- | Specifies the Object Lock rule for the specified object. Enable the this
-- rule when you apply @ObjectLockConfiguration@ to a bucket. Bucket
-- settings require both a mode and a period. The period can be either
-- @Days@ or @Years@ but you must select one. You cannot specify @Days@ and
-- @Years@ at the same time.
objectLockConfiguration_rule :: Lens.Lens' ObjectLockConfiguration (Prelude.Maybe ObjectLockRule)
objectLockConfiguration_rule = Lens.lens (\ObjectLockConfiguration' {rule} -> rule) (\s@ObjectLockConfiguration' {} a -> s {rule = a} :: ObjectLockConfiguration)

instance Data.FromXML ObjectLockConfiguration where
  parseXML x =
    ObjectLockConfiguration'
      Prelude.<$> (x Data..@? "ObjectLockEnabled")
      Prelude.<*> (x Data..@? "Rule")

instance Prelude.Hashable ObjectLockConfiguration where
  hashWithSalt _salt ObjectLockConfiguration' {..} =
    _salt `Prelude.hashWithSalt` objectLockEnabled
      `Prelude.hashWithSalt` rule

instance Prelude.NFData ObjectLockConfiguration where
  rnf ObjectLockConfiguration' {..} =
    Prelude.rnf objectLockEnabled
      `Prelude.seq` Prelude.rnf rule

instance Data.ToXML ObjectLockConfiguration where
  toXML ObjectLockConfiguration' {..} =
    Prelude.mconcat
      [ "ObjectLockEnabled" Data.@= objectLockEnabled,
        "Rule" Data.@= rule
      ]
