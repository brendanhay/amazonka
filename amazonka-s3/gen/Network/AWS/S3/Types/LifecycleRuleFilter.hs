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
-- Module      : Network.AWS.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies
-- to. A @Filter@ must have exactly one of @Prefix@, @Tag@, or @And@
-- specified.
--
-- /See:/ 'newLifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { -- | Prefix identifying one or more objects to which the rule applies.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Core.Maybe Core.Text,
    and :: Core.Maybe LifecycleRuleAndOperator,
    -- | This tag must exist in the object\'s tag set in order for the rule to
    -- apply.
    tag :: Core.Maybe Tag
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LifecycleRuleFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'lifecycleRuleFilter_prefix' - Prefix identifying one or more objects to which the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'and', 'lifecycleRuleFilter_and' - Undocumented member.
--
-- 'tag', 'lifecycleRuleFilter_tag' - This tag must exist in the object\'s tag set in order for the rule to
-- apply.
newLifecycleRuleFilter ::
  LifecycleRuleFilter
newLifecycleRuleFilter =
  LifecycleRuleFilter'
    { prefix = Core.Nothing,
      and = Core.Nothing,
      tag = Core.Nothing
    }

-- | Prefix identifying one or more objects to which the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRuleFilter_prefix :: Lens.Lens' LifecycleRuleFilter (Core.Maybe Core.Text)
lifecycleRuleFilter_prefix = Lens.lens (\LifecycleRuleFilter' {prefix} -> prefix) (\s@LifecycleRuleFilter' {} a -> s {prefix = a} :: LifecycleRuleFilter)

-- | Undocumented member.
lifecycleRuleFilter_and :: Lens.Lens' LifecycleRuleFilter (Core.Maybe LifecycleRuleAndOperator)
lifecycleRuleFilter_and = Lens.lens (\LifecycleRuleFilter' {and} -> and) (\s@LifecycleRuleFilter' {} a -> s {and = a} :: LifecycleRuleFilter)

-- | This tag must exist in the object\'s tag set in order for the rule to
-- apply.
lifecycleRuleFilter_tag :: Lens.Lens' LifecycleRuleFilter (Core.Maybe Tag)
lifecycleRuleFilter_tag = Lens.lens (\LifecycleRuleFilter' {tag} -> tag) (\s@LifecycleRuleFilter' {} a -> s {tag = a} :: LifecycleRuleFilter)

instance Core.FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "And")
      Core.<*> (x Core..@? "Tag")

instance Core.Hashable LifecycleRuleFilter

instance Core.NFData LifecycleRuleFilter

instance Core.ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter' {..} =
    Core.mconcat
      [ "Prefix" Core.@= prefix,
        "And" Core.@= and,
        "Tag" Core.@= tag
      ]
