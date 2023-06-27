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
-- Module      : Amazonka.Connect.Types.ResourceTagsSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ResourceTagsSearchCriteria where

import Amazonka.Connect.Types.TagSearchCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to search tags.
--
-- /See:/ 'newResourceTagsSearchCriteria' smart constructor.
data ResourceTagsSearchCriteria = ResourceTagsSearchCriteria'
  { -- | The search criteria to be used to return tags.
    tagSearchCondition :: Prelude.Maybe TagSearchCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTagsSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSearchCondition', 'resourceTagsSearchCriteria_tagSearchCondition' - The search criteria to be used to return tags.
newResourceTagsSearchCriteria ::
  ResourceTagsSearchCriteria
newResourceTagsSearchCriteria =
  ResourceTagsSearchCriteria'
    { tagSearchCondition =
        Prelude.Nothing
    }

-- | The search criteria to be used to return tags.
resourceTagsSearchCriteria_tagSearchCondition :: Lens.Lens' ResourceTagsSearchCriteria (Prelude.Maybe TagSearchCondition)
resourceTagsSearchCriteria_tagSearchCondition = Lens.lens (\ResourceTagsSearchCriteria' {tagSearchCondition} -> tagSearchCondition) (\s@ResourceTagsSearchCriteria' {} a -> s {tagSearchCondition = a} :: ResourceTagsSearchCriteria)

instance Prelude.Hashable ResourceTagsSearchCriteria where
  hashWithSalt _salt ResourceTagsSearchCriteria' {..} =
    _salt `Prelude.hashWithSalt` tagSearchCondition

instance Prelude.NFData ResourceTagsSearchCriteria where
  rnf ResourceTagsSearchCriteria' {..} =
    Prelude.rnf tagSearchCondition

instance Data.ToJSON ResourceTagsSearchCriteria where
  toJSON ResourceTagsSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TagSearchCondition" Data..=)
              Prelude.<$> tagSearchCondition
          ]
      )
