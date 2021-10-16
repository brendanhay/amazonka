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
-- Module      : Network.AWS.Transcribe.Types.CategoryProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.CategoryProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.Rule

-- | An object that contains the rules and additional information about a
-- call analytics category.
--
-- /See:/ 'newCategoryProperties' smart constructor.
data CategoryProperties = CategoryProperties'
  { -- | A timestamp that shows when the call analytics category was most
    -- recently updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The rules used to create a call analytics category.
    rules :: Prelude.Maybe (Prelude.NonEmpty Rule),
    -- | The name of the call analytics category.
    categoryName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that shows when the call analytics category was created.
    createTime :: Prelude.Maybe Core.POSIX
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
-- 'lastUpdateTime', 'categoryProperties_lastUpdateTime' - A timestamp that shows when the call analytics category was most
-- recently updated.
--
-- 'rules', 'categoryProperties_rules' - The rules used to create a call analytics category.
--
-- 'categoryName', 'categoryProperties_categoryName' - The name of the call analytics category.
--
-- 'createTime', 'categoryProperties_createTime' - A timestamp that shows when the call analytics category was created.
newCategoryProperties ::
  CategoryProperties
newCategoryProperties =
  CategoryProperties'
    { lastUpdateTime =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      categoryName = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | A timestamp that shows when the call analytics category was most
-- recently updated.
categoryProperties_lastUpdateTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_lastUpdateTime = Lens.lens (\CategoryProperties' {lastUpdateTime} -> lastUpdateTime) (\s@CategoryProperties' {} a -> s {lastUpdateTime = a} :: CategoryProperties) Prelude.. Lens.mapping Core._Time

-- | The rules used to create a call analytics category.
categoryProperties_rules :: Lens.Lens' CategoryProperties (Prelude.Maybe (Prelude.NonEmpty Rule))
categoryProperties_rules = Lens.lens (\CategoryProperties' {rules} -> rules) (\s@CategoryProperties' {} a -> s {rules = a} :: CategoryProperties) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the call analytics category.
categoryProperties_categoryName :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.Text)
categoryProperties_categoryName = Lens.lens (\CategoryProperties' {categoryName} -> categoryName) (\s@CategoryProperties' {} a -> s {categoryName = a} :: CategoryProperties)

-- | A timestamp that shows when the call analytics category was created.
categoryProperties_createTime :: Lens.Lens' CategoryProperties (Prelude.Maybe Prelude.UTCTime)
categoryProperties_createTime = Lens.lens (\CategoryProperties' {createTime} -> createTime) (\s@CategoryProperties' {} a -> s {createTime = a} :: CategoryProperties) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CategoryProperties where
  parseJSON =
    Core.withObject
      "CategoryProperties"
      ( \x ->
          CategoryProperties'
            Prelude.<$> (x Core..:? "LastUpdateTime")
            Prelude.<*> (x Core..:? "Rules")
            Prelude.<*> (x Core..:? "CategoryName")
            Prelude.<*> (x Core..:? "CreateTime")
      )

instance Prelude.Hashable CategoryProperties

instance Prelude.NFData CategoryProperties
