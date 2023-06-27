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
-- Module      : Amazonka.Connect.Types.TagSearchCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TagSearchCondition where

import Amazonka.Connect.Types.StringComparisonType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return tags.
--
-- /See:/ 'newTagSearchCondition' smart constructor.
data TagSearchCondition = TagSearchCondition'
  { -- | The tag key used in the tag search condition.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | The type of comparison to be made when evaluating the tag key in tag
    -- search condition.
    tagKeyComparisonType :: Prelude.Maybe StringComparisonType,
    -- | The tag value used in the tag search condition.
    tagValue :: Prelude.Maybe Prelude.Text,
    -- | The type of comparison to be made when evaluating the tag value in tag
    -- search condition.
    tagValueComparisonType :: Prelude.Maybe StringComparisonType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagSearchCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'tagSearchCondition_tagKey' - The tag key used in the tag search condition.
--
-- 'tagKeyComparisonType', 'tagSearchCondition_tagKeyComparisonType' - The type of comparison to be made when evaluating the tag key in tag
-- search condition.
--
-- 'tagValue', 'tagSearchCondition_tagValue' - The tag value used in the tag search condition.
--
-- 'tagValueComparisonType', 'tagSearchCondition_tagValueComparisonType' - The type of comparison to be made when evaluating the tag value in tag
-- search condition.
newTagSearchCondition ::
  TagSearchCondition
newTagSearchCondition =
  TagSearchCondition'
    { tagKey = Prelude.Nothing,
      tagKeyComparisonType = Prelude.Nothing,
      tagValue = Prelude.Nothing,
      tagValueComparisonType = Prelude.Nothing
    }

-- | The tag key used in the tag search condition.
tagSearchCondition_tagKey :: Lens.Lens' TagSearchCondition (Prelude.Maybe Prelude.Text)
tagSearchCondition_tagKey = Lens.lens (\TagSearchCondition' {tagKey} -> tagKey) (\s@TagSearchCondition' {} a -> s {tagKey = a} :: TagSearchCondition)

-- | The type of comparison to be made when evaluating the tag key in tag
-- search condition.
tagSearchCondition_tagKeyComparisonType :: Lens.Lens' TagSearchCondition (Prelude.Maybe StringComparisonType)
tagSearchCondition_tagKeyComparisonType = Lens.lens (\TagSearchCondition' {tagKeyComparisonType} -> tagKeyComparisonType) (\s@TagSearchCondition' {} a -> s {tagKeyComparisonType = a} :: TagSearchCondition)

-- | The tag value used in the tag search condition.
tagSearchCondition_tagValue :: Lens.Lens' TagSearchCondition (Prelude.Maybe Prelude.Text)
tagSearchCondition_tagValue = Lens.lens (\TagSearchCondition' {tagValue} -> tagValue) (\s@TagSearchCondition' {} a -> s {tagValue = a} :: TagSearchCondition)

-- | The type of comparison to be made when evaluating the tag value in tag
-- search condition.
tagSearchCondition_tagValueComparisonType :: Lens.Lens' TagSearchCondition (Prelude.Maybe StringComparisonType)
tagSearchCondition_tagValueComparisonType = Lens.lens (\TagSearchCondition' {tagValueComparisonType} -> tagValueComparisonType) (\s@TagSearchCondition' {} a -> s {tagValueComparisonType = a} :: TagSearchCondition)

instance Prelude.Hashable TagSearchCondition where
  hashWithSalt _salt TagSearchCondition' {..} =
    _salt
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagKeyComparisonType
      `Prelude.hashWithSalt` tagValue
      `Prelude.hashWithSalt` tagValueComparisonType

instance Prelude.NFData TagSearchCondition where
  rnf TagSearchCondition' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagKeyComparisonType
      `Prelude.seq` Prelude.rnf tagValue
      `Prelude.seq` Prelude.rnf tagValueComparisonType

instance Data.ToJSON TagSearchCondition where
  toJSON TagSearchCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tagKey" Data..=) Prelude.<$> tagKey,
            ("tagKeyComparisonType" Data..=)
              Prelude.<$> tagKeyComparisonType,
            ("tagValue" Data..=) Prelude.<$> tagValue,
            ("tagValueComparisonType" Data..=)
              Prelude.<$> tagValueComparisonType
          ]
      )
