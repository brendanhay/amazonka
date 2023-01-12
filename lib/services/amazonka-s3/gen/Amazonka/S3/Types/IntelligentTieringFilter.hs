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
-- Module      : Amazonka.S3.Types.IntelligentTieringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IntelligentTieringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.IntelligentTieringAndOperator
import Amazonka.S3.Types.Tag

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering
-- configuration applies to.
--
-- /See:/ 'newIntelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { -- | A conjunction (logical AND) of predicates, which is used in evaluating a
    -- metrics filter. The operator must have at least two predicates, and an
    -- object must match all of the predicates in order for the filter to
    -- apply.
    and :: Prelude.Maybe IntelligentTieringAndOperator,
    -- | An object key name prefix that identifies the subset of objects to which
    -- the rule applies.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntelligentTieringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'intelligentTieringFilter_and' - A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- 'prefix', 'intelligentTieringFilter_prefix' - An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'tag', 'intelligentTieringFilter_tag' - Undocumented member.
newIntelligentTieringFilter ::
  IntelligentTieringFilter
newIntelligentTieringFilter =
  IntelligentTieringFilter'
    { and = Prelude.Nothing,
      prefix = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
intelligentTieringFilter_and :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe IntelligentTieringAndOperator)
intelligentTieringFilter_and = Lens.lens (\IntelligentTieringFilter' {and} -> and) (\s@IntelligentTieringFilter' {} a -> s {and = a} :: IntelligentTieringFilter)

-- | An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
intelligentTieringFilter_prefix :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe Prelude.Text)
intelligentTieringFilter_prefix = Lens.lens (\IntelligentTieringFilter' {prefix} -> prefix) (\s@IntelligentTieringFilter' {} a -> s {prefix = a} :: IntelligentTieringFilter)

-- | Undocumented member.
intelligentTieringFilter_tag :: Lens.Lens' IntelligentTieringFilter (Prelude.Maybe Tag)
intelligentTieringFilter_tag = Lens.lens (\IntelligentTieringFilter' {tag} -> tag) (\s@IntelligentTieringFilter' {} a -> s {tag = a} :: IntelligentTieringFilter)

instance Data.FromXML IntelligentTieringFilter where
  parseXML x =
    IntelligentTieringFilter'
      Prelude.<$> (x Data..@? "And")
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> (x Data..@? "Tag")

instance Prelude.Hashable IntelligentTieringFilter where
  hashWithSalt _salt IntelligentTieringFilter' {..} =
    _salt `Prelude.hashWithSalt` and
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` tag

instance Prelude.NFData IntelligentTieringFilter where
  rnf IntelligentTieringFilter' {..} =
    Prelude.rnf and
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf tag

instance Data.ToXML IntelligentTieringFilter where
  toXML IntelligentTieringFilter' {..} =
    Prelude.mconcat
      [ "And" Data.@= and,
        "Prefix" Data.@= prefix,
        "Tag" Data.@= tag
      ]
