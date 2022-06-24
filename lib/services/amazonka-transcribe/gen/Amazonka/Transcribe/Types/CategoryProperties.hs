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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CategoryProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.Rule

-- | An object that contains the rules and additional information about a
-- call analytics category.
--
-- /See:/ 'newCategoryProperties' smart constructor.
data CategoryProperties = CategoryProperties'
  { -- | The rules used to create a call analytics category.
    rules :: Prelude.Maybe (Prelude.NonEmpty Rule),
    -- | A timestamp that shows when the call analytics category was most
    -- recently updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that shows when the call analytics category was created.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the call analytics category.
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
-- 'rules', 'categoryProperties_rules' - The rules used to create a call analytics category.
--
-- 'lastUpdateTime', 'categoryProperties_lastUpdateTime' - A timestamp that shows when the call analytics category was most
-- recently updated.
--
-- 'createTime', 'categoryProperties_createTime' - A timestamp that shows when the call analytics category was created.
--
-- 'categoryName', 'categoryProperties_categoryName' - The name of the call analytics category.
newCategoryProperties ::
  CategoryProperties
newCategoryProperties =
  CategoryProperties'
    { rules = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      categoryName = Prelude.Nothing
    }

-- | The rules used to create a call analytics category.
categoryProperties_rules :: Lens.Lens' CategoryProperties (Prelude.Maybe (Prelude.NonEmpty Rule))
categoryProperties_rules = Lens.lens (\CategoryProperties' {rules} -> rules) (\s@CategoryProperties' {} a -> s {rules = a} :: CategoryProperties) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that shows when the call analytics category was most
-- recently updated.
categoryProperties_lastUpdateTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_lastUpdateTime = Lens.lens (\CategoryProperties' {lastUpdateTime} -> lastUpdateTime) (\s@CategoryProperties' {} a -> s {lastUpdateTime = a} :: CategoryProperties) Prelude.. Lens.mapping Core._Time

-- | A timestamp that shows when the call analytics category was created.
categoryProperties_createTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_createTime = Lens.lens (\CategoryProperties' {createTime} -> createTime) (\s@CategoryProperties' {} a -> s {createTime = a} :: CategoryProperties) Prelude.. Lens.mapping Core._Time

-- | The name of the call analytics category.
categoryProperties_categoryName :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.Text)
categoryProperties_categoryName = Lens.lens (\CategoryProperties' {categoryName} -> categoryName) (\s@CategoryProperties' {} a -> s {categoryName = a} :: CategoryProperties)

instance Core.FromJSON CategoryProperties where
  parseJSON =
    Core.withObject
      "CategoryProperties"
      ( \x ->
          CategoryProperties'
            Prelude.<$> (x Core..:? "Rules")
            Prelude.<*> (x Core..:? "LastUpdateTime")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "CategoryName")
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
