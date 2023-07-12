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
-- Module      : Amazonka.WellArchitected.Types.Choice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Choice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.AdditionalResources
import Amazonka.WellArchitected.Types.ChoiceContent

-- | A choice available to answer question.
--
-- /See:/ 'newChoice' smart constructor.
data Choice = Choice'
  { -- | The additional resources for a choice. A choice can have up to two
    -- additional resources: one of type @HELPFUL_RESOURCE@, one of type
    -- @IMPROVEMENT_PLAN@, or both.
    additionalResources :: Prelude.Maybe [AdditionalResources],
    choiceId :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    -- | The choice level helpful resource.
    helpfulResource :: Prelude.Maybe ChoiceContent,
    -- | The choice level improvement plan.
    improvementPlan :: Prelude.Maybe ChoiceContent,
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Choice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalResources', 'choice_additionalResources' - The additional resources for a choice. A choice can have up to two
-- additional resources: one of type @HELPFUL_RESOURCE@, one of type
-- @IMPROVEMENT_PLAN@, or both.
--
-- 'choiceId', 'choice_choiceId' - Undocumented member.
--
-- 'description', 'choice_description' - Undocumented member.
--
-- 'helpfulResource', 'choice_helpfulResource' - The choice level helpful resource.
--
-- 'improvementPlan', 'choice_improvementPlan' - The choice level improvement plan.
--
-- 'title', 'choice_title' - Undocumented member.
newChoice ::
  Choice
newChoice =
  Choice'
    { additionalResources = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      description = Prelude.Nothing,
      helpfulResource = Prelude.Nothing,
      improvementPlan = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The additional resources for a choice. A choice can have up to two
-- additional resources: one of type @HELPFUL_RESOURCE@, one of type
-- @IMPROVEMENT_PLAN@, or both.
choice_additionalResources :: Lens.Lens' Choice (Prelude.Maybe [AdditionalResources])
choice_additionalResources = Lens.lens (\Choice' {additionalResources} -> additionalResources) (\s@Choice' {} a -> s {additionalResources = a} :: Choice) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
choice_choiceId :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_choiceId = Lens.lens (\Choice' {choiceId} -> choiceId) (\s@Choice' {} a -> s {choiceId = a} :: Choice)

-- | Undocumented member.
choice_description :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_description = Lens.lens (\Choice' {description} -> description) (\s@Choice' {} a -> s {description = a} :: Choice)

-- | The choice level helpful resource.
choice_helpfulResource :: Lens.Lens' Choice (Prelude.Maybe ChoiceContent)
choice_helpfulResource = Lens.lens (\Choice' {helpfulResource} -> helpfulResource) (\s@Choice' {} a -> s {helpfulResource = a} :: Choice)

-- | The choice level improvement plan.
choice_improvementPlan :: Lens.Lens' Choice (Prelude.Maybe ChoiceContent)
choice_improvementPlan = Lens.lens (\Choice' {improvementPlan} -> improvementPlan) (\s@Choice' {} a -> s {improvementPlan = a} :: Choice)

-- | Undocumented member.
choice_title :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_title = Lens.lens (\Choice' {title} -> title) (\s@Choice' {} a -> s {title = a} :: Choice)

instance Data.FromJSON Choice where
  parseJSON =
    Data.withObject
      "Choice"
      ( \x ->
          Choice'
            Prelude.<$> ( x
                            Data..:? "AdditionalResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HelpfulResource")
            Prelude.<*> (x Data..:? "ImprovementPlan")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable Choice where
  hashWithSalt _salt Choice' {..} =
    _salt
      `Prelude.hashWithSalt` additionalResources
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` helpfulResource
      `Prelude.hashWithSalt` improvementPlan
      `Prelude.hashWithSalt` title

instance Prelude.NFData Choice where
  rnf Choice' {..} =
    Prelude.rnf additionalResources
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf helpfulResource
      `Prelude.seq` Prelude.rnf improvementPlan
      `Prelude.seq` Prelude.rnf title
