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
-- Module      : Amazonka.Transcribe.Types.CategoryProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CategoryProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.Rule

-- | Provides you with the properties of the Call Analytics category you
-- specified in your request. This includes the list of rules that define
-- the specified category.
--
-- /See:/ 'newCategoryProperties' smart constructor.
data CategoryProperties = CategoryProperties'
  { -- | The rules used to define a Call Analytics category. Each category can
    -- have between 1 and 20 rules.
    rules :: Prelude.Maybe (Prelude.NonEmpty Rule),
    -- | The date and time the specified Call Analytics category was last
    -- updated.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-05T12:45:32.691000-07:00@ represents 12:45 PM UTC-7 on
    -- May 5, 2022.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time the specified Call Analytics category was created.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the Call Analytics category. Category names are case
    -- sensitive and must be unique within an Amazon Web Services account.
    categoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoryProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'categoryProperties_rules' - The rules used to define a Call Analytics category. Each category can
-- have between 1 and 20 rules.
--
-- 'lastUpdateTime', 'categoryProperties_lastUpdateTime' - The date and time the specified Call Analytics category was last
-- updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-05T12:45:32.691000-07:00@ represents 12:45 PM UTC-7 on
-- May 5, 2022.
--
-- 'createTime', 'categoryProperties_createTime' - The date and time the specified Call Analytics category was created.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'categoryName', 'categoryProperties_categoryName' - The name of the Call Analytics category. Category names are case
-- sensitive and must be unique within an Amazon Web Services account.
newCategoryProperties ::
  CategoryProperties
newCategoryProperties =
  CategoryProperties'
    { rules = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      categoryName = Prelude.Nothing
    }

-- | The rules used to define a Call Analytics category. Each category can
-- have between 1 and 20 rules.
categoryProperties_rules :: Lens.Lens' CategoryProperties (Prelude.Maybe (Prelude.NonEmpty Rule))
categoryProperties_rules = Lens.lens (\CategoryProperties' {rules} -> rules) (\s@CategoryProperties' {} a -> s {rules = a} :: CategoryProperties) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the specified Call Analytics category was last
-- updated.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-05T12:45:32.691000-07:00@ represents 12:45 PM UTC-7 on
-- May 5, 2022.
categoryProperties_lastUpdateTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_lastUpdateTime = Lens.lens (\CategoryProperties' {lastUpdateTime} -> lastUpdateTime) (\s@CategoryProperties' {} a -> s {lastUpdateTime = a} :: CategoryProperties) Prelude.. Lens.mapping Data._Time

-- | The date and time the specified Call Analytics category was created.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
categoryProperties_createTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_createTime = Lens.lens (\CategoryProperties' {createTime} -> createTime) (\s@CategoryProperties' {} a -> s {createTime = a} :: CategoryProperties) Prelude.. Lens.mapping Data._Time

-- | The name of the Call Analytics category. Category names are case
-- sensitive and must be unique within an Amazon Web Services account.
categoryProperties_categoryName :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.Text)
categoryProperties_categoryName = Lens.lens (\CategoryProperties' {categoryName} -> categoryName) (\s@CategoryProperties' {} a -> s {categoryName = a} :: CategoryProperties)

instance Data.FromJSON CategoryProperties where
  parseJSON =
    Data.withObject
      "CategoryProperties"
      ( \x ->
          CategoryProperties'
            Prelude.<$> (x Data..:? "Rules")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "CategoryName")
      )

instance Prelude.Hashable CategoryProperties where
  hashWithSalt _salt CategoryProperties' {..} =
    _salt `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` categoryName

instance Prelude.NFData CategoryProperties where
  rnf CategoryProperties' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf categoryName
