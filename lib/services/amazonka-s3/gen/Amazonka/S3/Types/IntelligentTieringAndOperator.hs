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
-- Module      : Amazonka.S3.Types.IntelligentTieringAndOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IntelligentTieringAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | A container for specifying S3 Intelligent-Tiering filters. The filters
-- determine the subset of objects to which the rule applies.
--
-- /See:/ 'newIntelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
  { -- | All of these tags must exist in the object\'s tag set in order for the
    -- configuration to apply.
    tags :: Prelude.Maybe [Tag],
    -- | An object key name prefix that identifies the subset of objects to which
    -- the configuration applies.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntelligentTieringAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'intelligentTieringAndOperator_tags' - All of these tags must exist in the object\'s tag set in order for the
-- configuration to apply.
--
-- 'prefix', 'intelligentTieringAndOperator_prefix' - An object key name prefix that identifies the subset of objects to which
-- the configuration applies.
newIntelligentTieringAndOperator ::
  IntelligentTieringAndOperator
newIntelligentTieringAndOperator =
  IntelligentTieringAndOperator'
    { tags =
        Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | All of these tags must exist in the object\'s tag set in order for the
-- configuration to apply.
intelligentTieringAndOperator_tags :: Lens.Lens' IntelligentTieringAndOperator (Prelude.Maybe [Tag])
intelligentTieringAndOperator_tags = Lens.lens (\IntelligentTieringAndOperator' {tags} -> tags) (\s@IntelligentTieringAndOperator' {} a -> s {tags = a} :: IntelligentTieringAndOperator) Prelude.. Lens.mapping Lens.coerced

-- | An object key name prefix that identifies the subset of objects to which
-- the configuration applies.
intelligentTieringAndOperator_prefix :: Lens.Lens' IntelligentTieringAndOperator (Prelude.Maybe Prelude.Text)
intelligentTieringAndOperator_prefix = Lens.lens (\IntelligentTieringAndOperator' {prefix} -> prefix) (\s@IntelligentTieringAndOperator' {} a -> s {prefix = a} :: IntelligentTieringAndOperator)

instance Core.FromXML IntelligentTieringAndOperator where
  parseXML x =
    IntelligentTieringAndOperator'
      Prelude.<$> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "Prefix")

instance
  Prelude.Hashable
    IntelligentTieringAndOperator
  where
  hashWithSalt _salt IntelligentTieringAndOperator' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData IntelligentTieringAndOperator where
  rnf IntelligentTieringAndOperator' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf prefix

instance Core.ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator' {..} =
    Prelude.mconcat
      [ "Tag"
          Core.@= Core.toXML (Core.toXMLList "Tag" Prelude.<$> tags),
        "Prefix" Core.@= prefix
      ]
