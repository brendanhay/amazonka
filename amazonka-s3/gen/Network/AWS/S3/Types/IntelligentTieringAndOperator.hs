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
-- Module      : Network.AWS.S3.Types.IntelligentTieringAndOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringAndOperator where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
intelligentTieringAndOperator_tags = Lens.lens (\IntelligentTieringAndOperator' {tags} -> tags) (\s@IntelligentTieringAndOperator' {} a -> s {tags = a} :: IntelligentTieringAndOperator) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    IntelligentTieringAndOperator
  where
  parseXML x =
    IntelligentTieringAndOperator'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> ( x Prelude..@? "Tag" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance
  Prelude.Hashable
    IntelligentTieringAndOperator

instance Prelude.NFData IntelligentTieringAndOperator

instance Prelude.ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "Tag"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Tag" Prelude.<$> tags)
      ]
