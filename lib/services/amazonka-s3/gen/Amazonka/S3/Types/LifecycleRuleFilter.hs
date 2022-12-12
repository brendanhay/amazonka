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
-- Module      : Amazonka.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LifecycleRuleFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.LifecycleRuleAndOperator
import Amazonka.S3.Types.Tag

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies
-- to. A @Filter@ must have exactly one of @Prefix@, @Tag@, or @And@
-- specified.
--
-- /See:/ 'newLifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { and :: Prelude.Maybe LifecycleRuleAndOperator,
    -- | Minimum object size to which the rule applies.
    objectSizeGreaterThan :: Prelude.Maybe Prelude.Integer,
    -- | Maximum object size to which the rule applies.
    objectSizeLessThan :: Prelude.Maybe Prelude.Integer,
    -- | Prefix identifying one or more objects to which the rule applies.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | This tag must exist in the object\'s tag set in order for the rule to
    -- apply.
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleRuleFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'lifecycleRuleFilter_and' - Undocumented member.
--
-- 'objectSizeGreaterThan', 'lifecycleRuleFilter_objectSizeGreaterThan' - Minimum object size to which the rule applies.
--
-- 'objectSizeLessThan', 'lifecycleRuleFilter_objectSizeLessThan' - Maximum object size to which the rule applies.
--
-- 'prefix', 'lifecycleRuleFilter_prefix' - Prefix identifying one or more objects to which the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'tag', 'lifecycleRuleFilter_tag' - This tag must exist in the object\'s tag set in order for the rule to
-- apply.
newLifecycleRuleFilter ::
  LifecycleRuleFilter
newLifecycleRuleFilter =
  LifecycleRuleFilter'
    { and = Prelude.Nothing,
      objectSizeGreaterThan = Prelude.Nothing,
      objectSizeLessThan = Prelude.Nothing,
      prefix = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | Undocumented member.
lifecycleRuleFilter_and :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe LifecycleRuleAndOperator)
lifecycleRuleFilter_and = Lens.lens (\LifecycleRuleFilter' {and} -> and) (\s@LifecycleRuleFilter' {} a -> s {and = a} :: LifecycleRuleFilter)

-- | Minimum object size to which the rule applies.
lifecycleRuleFilter_objectSizeGreaterThan :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Prelude.Integer)
lifecycleRuleFilter_objectSizeGreaterThan = Lens.lens (\LifecycleRuleFilter' {objectSizeGreaterThan} -> objectSizeGreaterThan) (\s@LifecycleRuleFilter' {} a -> s {objectSizeGreaterThan = a} :: LifecycleRuleFilter)

-- | Maximum object size to which the rule applies.
lifecycleRuleFilter_objectSizeLessThan :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Prelude.Integer)
lifecycleRuleFilter_objectSizeLessThan = Lens.lens (\LifecycleRuleFilter' {objectSizeLessThan} -> objectSizeLessThan) (\s@LifecycleRuleFilter' {} a -> s {objectSizeLessThan = a} :: LifecycleRuleFilter)

-- | Prefix identifying one or more objects to which the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRuleFilter_prefix :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Prelude.Text)
lifecycleRuleFilter_prefix = Lens.lens (\LifecycleRuleFilter' {prefix} -> prefix) (\s@LifecycleRuleFilter' {} a -> s {prefix = a} :: LifecycleRuleFilter)

-- | This tag must exist in the object\'s tag set in order for the rule to
-- apply.
lifecycleRuleFilter_tag :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Tag)
lifecycleRuleFilter_tag = Lens.lens (\LifecycleRuleFilter' {tag} -> tag) (\s@LifecycleRuleFilter' {} a -> s {tag = a} :: LifecycleRuleFilter)

instance Data.FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      Prelude.<$> (x Data..@? "And")
      Prelude.<*> (x Data..@? "ObjectSizeGreaterThan")
      Prelude.<*> (x Data..@? "ObjectSizeLessThan")
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> (x Data..@? "Tag")

instance Prelude.Hashable LifecycleRuleFilter where
  hashWithSalt _salt LifecycleRuleFilter' {..} =
    _salt `Prelude.hashWithSalt` and
      `Prelude.hashWithSalt` objectSizeGreaterThan
      `Prelude.hashWithSalt` objectSizeLessThan
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` tag

instance Prelude.NFData LifecycleRuleFilter where
  rnf LifecycleRuleFilter' {..} =
    Prelude.rnf and
      `Prelude.seq` Prelude.rnf objectSizeGreaterThan
      `Prelude.seq` Prelude.rnf objectSizeLessThan
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf tag

instance Data.ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter' {..} =
    Prelude.mconcat
      [ "And" Data.@= and,
        "ObjectSizeGreaterThan"
          Data.@= objectSizeGreaterThan,
        "ObjectSizeLessThan" Data.@= objectSizeLessThan,
        "Prefix" Data.@= prefix,
        "Tag" Data.@= tag
      ]
