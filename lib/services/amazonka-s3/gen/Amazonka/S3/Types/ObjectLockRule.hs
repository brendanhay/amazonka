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
-- Module      : Amazonka.S3.Types.ObjectLockRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectLockRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.DefaultRetention

-- | The container element for an Object Lock rule.
--
-- /See:/ 'newObjectLockRule' smart constructor.
data ObjectLockRule = ObjectLockRule'
  { -- | The default Object Lock retention mode and period that you want to apply
    -- to new objects placed in the specified bucket. Bucket settings require
    -- both a mode and a period. The period can be either @Days@ or @Years@ but
    -- you must select one. You cannot specify @Days@ and @Years@ at the same
    -- time.
    defaultRetention :: Prelude.Maybe DefaultRetention
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectLockRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultRetention', 'objectLockRule_defaultRetention' - The default Object Lock retention mode and period that you want to apply
-- to new objects placed in the specified bucket. Bucket settings require
-- both a mode and a period. The period can be either @Days@ or @Years@ but
-- you must select one. You cannot specify @Days@ and @Years@ at the same
-- time.
newObjectLockRule ::
  ObjectLockRule
newObjectLockRule =
  ObjectLockRule' {defaultRetention = Prelude.Nothing}

-- | The default Object Lock retention mode and period that you want to apply
-- to new objects placed in the specified bucket. Bucket settings require
-- both a mode and a period. The period can be either @Days@ or @Years@ but
-- you must select one. You cannot specify @Days@ and @Years@ at the same
-- time.
objectLockRule_defaultRetention :: Lens.Lens' ObjectLockRule (Prelude.Maybe DefaultRetention)
objectLockRule_defaultRetention = Lens.lens (\ObjectLockRule' {defaultRetention} -> defaultRetention) (\s@ObjectLockRule' {} a -> s {defaultRetention = a} :: ObjectLockRule)

instance Core.FromXML ObjectLockRule where
  parseXML x =
    ObjectLockRule'
      Prelude.<$> (x Core..@? "DefaultRetention")

instance Prelude.Hashable ObjectLockRule where
  hashWithSalt _salt ObjectLockRule' {..} =
    _salt `Prelude.hashWithSalt` defaultRetention

instance Prelude.NFData ObjectLockRule where
  rnf ObjectLockRule' {..} =
    Prelude.rnf defaultRetention

instance Core.ToXML ObjectLockRule where
  toXML ObjectLockRule' {..} =
    Prelude.mconcat
      ["DefaultRetention" Core.@= defaultRetention]
