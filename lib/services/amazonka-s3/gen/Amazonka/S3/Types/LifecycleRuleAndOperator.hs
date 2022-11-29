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
-- Module      : Amazonka.S3.Types.LifecycleRuleAndOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LifecycleRuleAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or
-- more predicates. The Lifecycle Rule will apply to any object matching
-- all of the predicates configured inside the And operator.
--
-- /See:/ 'newLifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { -- | All of these tags must exist in the object\'s tag set in order for the
    -- rule to apply.
    tags :: Prelude.Maybe [Tag],
    -- | Maximum object size to which the rule applies.
    objectSizeLessThan :: Prelude.Maybe Prelude.Integer,
    -- | Prefix identifying one or more objects to which the rule applies.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Minimum object size to which the rule applies.
    objectSizeGreaterThan :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleRuleAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'lifecycleRuleAndOperator_tags' - All of these tags must exist in the object\'s tag set in order for the
-- rule to apply.
--
-- 'objectSizeLessThan', 'lifecycleRuleAndOperator_objectSizeLessThan' - Maximum object size to which the rule applies.
--
-- 'prefix', 'lifecycleRuleAndOperator_prefix' - Prefix identifying one or more objects to which the rule applies.
--
-- 'objectSizeGreaterThan', 'lifecycleRuleAndOperator_objectSizeGreaterThan' - Minimum object size to which the rule applies.
newLifecycleRuleAndOperator ::
  LifecycleRuleAndOperator
newLifecycleRuleAndOperator =
  LifecycleRuleAndOperator'
    { tags = Prelude.Nothing,
      objectSizeLessThan = Prelude.Nothing,
      prefix = Prelude.Nothing,
      objectSizeGreaterThan = Prelude.Nothing
    }

-- | All of these tags must exist in the object\'s tag set in order for the
-- rule to apply.
lifecycleRuleAndOperator_tags :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe [Tag])
lifecycleRuleAndOperator_tags = Lens.lens (\LifecycleRuleAndOperator' {tags} -> tags) (\s@LifecycleRuleAndOperator' {} a -> s {tags = a} :: LifecycleRuleAndOperator) Prelude.. Lens.mapping Lens.coerced

-- | Maximum object size to which the rule applies.
lifecycleRuleAndOperator_objectSizeLessThan :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe Prelude.Integer)
lifecycleRuleAndOperator_objectSizeLessThan = Lens.lens (\LifecycleRuleAndOperator' {objectSizeLessThan} -> objectSizeLessThan) (\s@LifecycleRuleAndOperator' {} a -> s {objectSizeLessThan = a} :: LifecycleRuleAndOperator)

-- | Prefix identifying one or more objects to which the rule applies.
lifecycleRuleAndOperator_prefix :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe Prelude.Text)
lifecycleRuleAndOperator_prefix = Lens.lens (\LifecycleRuleAndOperator' {prefix} -> prefix) (\s@LifecycleRuleAndOperator' {} a -> s {prefix = a} :: LifecycleRuleAndOperator)

-- | Minimum object size to which the rule applies.
lifecycleRuleAndOperator_objectSizeGreaterThan :: Lens.Lens' LifecycleRuleAndOperator (Prelude.Maybe Prelude.Integer)
lifecycleRuleAndOperator_objectSizeGreaterThan = Lens.lens (\LifecycleRuleAndOperator' {objectSizeGreaterThan} -> objectSizeGreaterThan) (\s@LifecycleRuleAndOperator' {} a -> s {objectSizeGreaterThan = a} :: LifecycleRuleAndOperator)

instance Core.FromXML LifecycleRuleAndOperator where
  parseXML x =
    LifecycleRuleAndOperator'
      Prelude.<$> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "ObjectSizeLessThan")
      Prelude.<*> (x Core..@? "Prefix")
      Prelude.<*> (x Core..@? "ObjectSizeGreaterThan")

instance Prelude.Hashable LifecycleRuleAndOperator where
  hashWithSalt _salt LifecycleRuleAndOperator' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` objectSizeLessThan
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` objectSizeGreaterThan

instance Prelude.NFData LifecycleRuleAndOperator where
  rnf LifecycleRuleAndOperator' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf objectSizeLessThan
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf objectSizeGreaterThan

instance Core.ToXML LifecycleRuleAndOperator where
  toXML LifecycleRuleAndOperator' {..} =
    Prelude.mconcat
      [ "Tag"
          Core.@= Core.toXML (Core.toXMLList "Tag" Prelude.<$> tags),
        "ObjectSizeLessThan" Core.@= objectSizeLessThan,
        "Prefix" Core.@= prefix,
        "ObjectSizeGreaterThan"
          Core.@= objectSizeGreaterThan
      ]
