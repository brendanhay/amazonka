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
-- Module      : Network.AWS.S3.Types.IntelligentTieringFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering
-- configuration applies to.
--
-- /See:/ 'newIntelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { -- | An object key name prefix that identifies the subset of objects to which
    -- the rule applies.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A conjunction (logical AND) of predicates, which is used in evaluating a
    -- metrics filter. The operator must have at least two predicates, and an
    -- object must match all of the predicates in order for the filter to
    -- apply.
    and :: Prelude.Maybe IntelligentTieringAndOperator,
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IntelligentTieringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'intelligentTieringFilter_prefix' - An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'and', 'intelligentTieringFilter_and' - A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- 'tag', 'intelligentTieringFilter_tag' - Undocumented member.
newIntelligentTieringFilter ::
  IntelligentTieringFilter
newIntelligentTieringFilter =
  IntelligentTieringFilter'
    { prefix = Prelude.Nothing,
      and = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
intelligentTieringFilter_prefix :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe Prelude.Text)
intelligentTieringFilter_prefix = Lens.lens (\IntelligentTieringFilter' {prefix} -> prefix) (\s@IntelligentTieringFilter' {} a -> s {prefix = a} :: IntelligentTieringFilter)

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
intelligentTieringFilter_and :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe IntelligentTieringAndOperator)
intelligentTieringFilter_and = Lens.lens (\IntelligentTieringFilter' {and} -> and) (\s@IntelligentTieringFilter' {} a -> s {and = a} :: IntelligentTieringFilter)

-- | Undocumented member.
intelligentTieringFilter_tag :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe Tag)
intelligentTieringFilter_tag = Lens.lens (\IntelligentTieringFilter' {tag} -> tag) (\s@IntelligentTieringFilter' {} a -> s {tag = a} :: IntelligentTieringFilter)

instance Prelude.FromXML IntelligentTieringFilter where
  parseXML x =
    IntelligentTieringFilter'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> (x Prelude..@? "And")
      Prelude.<*> (x Prelude..@? "Tag")

instance Prelude.Hashable IntelligentTieringFilter

instance Prelude.NFData IntelligentTieringFilter

instance Prelude.ToXML IntelligentTieringFilter where
  toXML IntelligentTieringFilter' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "And" Prelude.@= and,
        "Tag" Prelude.@= tag
      ]
