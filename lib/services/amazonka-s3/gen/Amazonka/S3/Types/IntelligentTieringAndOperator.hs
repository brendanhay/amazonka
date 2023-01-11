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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IntelligentTieringAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | A container for specifying S3 Intelligent-Tiering filters. The filters
-- determine the subset of objects to which the rule applies.
--
-- /See:/ 'newIntelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
  { -- | An object key name prefix that identifies the subset of objects to which
    -- the configuration applies.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | All of these tags must exist in the object\'s tag set in order for the
    -- configuration to apply.
    tags :: Prelude.Maybe [Tag]
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
-- 'prefix', 'intelligentTieringAndOperator_prefix' - An object key name prefix that identifies the subset of objects to which
-- the configuration applies.
--
-- 'tags', 'intelligentTieringAndOperator_tags' - All of these tags must exist in the object\'s tag set in order for the
-- configuration to apply.
newIntelligentTieringAndOperator ::
  IntelligentTieringAndOperator
newIntelligentTieringAndOperator =
  IntelligentTieringAndOperator'
    { prefix =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which
-- the configuration applies.
intelligentTieringAndOperator_prefix :: Lens.Lens' IntelligentTieringAndOperator (Prelude.Maybe Prelude.Text)
intelligentTieringAndOperator_prefix = Lens.lens (\IntelligentTieringAndOperator' {prefix} -> prefix) (\s@IntelligentTieringAndOperator' {} a -> s {prefix = a} :: IntelligentTieringAndOperator)

-- | All of these tags must exist in the object\'s tag set in order for the
-- configuration to apply.
intelligentTieringAndOperator_tags :: Lens.Lens' IntelligentTieringAndOperator (Prelude.Maybe [Tag])
intelligentTieringAndOperator_tags = Lens.lens (\IntelligentTieringAndOperator' {tags} -> tags) (\s@IntelligentTieringAndOperator' {} a -> s {tags = a} :: IntelligentTieringAndOperator) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML IntelligentTieringAndOperator where
  parseXML x =
    IntelligentTieringAndOperator'
      Prelude.<$> (x Data..@? "Prefix")
      Prelude.<*> ( x Data..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )

instance
  Prelude.Hashable
    IntelligentTieringAndOperator
  where
  hashWithSalt _salt IntelligentTieringAndOperator' {..} =
    _salt `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` tags

instance Prelude.NFData IntelligentTieringAndOperator where
  rnf IntelligentTieringAndOperator' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf tags

instance Data.ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator' {..} =
    Prelude.mconcat
      [ "Prefix" Data.@= prefix,
        "Tag"
          Data.@= Data.toXML (Data.toXMLList "Tag" Prelude.<$> tags)
      ]
