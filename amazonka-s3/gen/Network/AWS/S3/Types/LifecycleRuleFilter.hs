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
-- Module      : Network.AWS.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    prefix :: Prelude.Maybe Prelude.Text,
    and :: Prelude.Maybe LifecycleRuleAndOperator,
    -- | This tag must exist in the object\'s tag set in order for the rule to
    -- apply.
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { prefix = Prelude.Nothing,
      and = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | Prefix identifying one or more objects to which the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRuleFilter_prefix :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Prelude.Text)
lifecycleRuleFilter_prefix = Lens.lens (\LifecycleRuleFilter' {prefix} -> prefix) (\s@LifecycleRuleFilter' {} a -> s {prefix = a} :: LifecycleRuleFilter)

-- | Undocumented member.
lifecycleRuleFilter_and :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe LifecycleRuleAndOperator)
lifecycleRuleFilter_and = Lens.lens (\LifecycleRuleFilter' {and} -> and) (\s@LifecycleRuleFilter' {} a -> s {and = a} :: LifecycleRuleFilter)

-- | This tag must exist in the object\'s tag set in order for the rule to
-- apply.
lifecycleRuleFilter_tag :: Lens.Lens' LifecycleRuleFilter (Prelude.Maybe Tag)
lifecycleRuleFilter_tag = Lens.lens (\LifecycleRuleFilter' {tag} -> tag) (\s@LifecycleRuleFilter' {} a -> s {tag = a} :: LifecycleRuleFilter)

instance Prelude.FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> (x Prelude..@? "And")
      Prelude.<*> (x Prelude..@? "Tag")

instance Prelude.Hashable LifecycleRuleFilter

instance Prelude.NFData LifecycleRuleFilter

instance Prelude.ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "And" Prelude.@= and,
        "Tag" Prelude.@= tag
      ]
