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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Choice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.AdditionalResources
import Amazonka.WellArchitected.Types.ChoiceContent

-- | A choice available to answer question.
--
-- /See:/ 'newChoice' smart constructor.
data Choice = Choice'
  { description :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text,
    title :: Prelude.Maybe Prelude.Text,
    -- | The choice level helpful resource.
    helpfulResource :: Prelude.Maybe ChoiceContent,
    -- | The additional resources for a choice. A choice can have up to two
    -- additional resources: one of type @HELPFUL_RESOURCE@, one of type
    -- @IMPROVEMENT_PLAN@, or both.
    additionalResources :: Prelude.Maybe [AdditionalResources],
    -- | The choice level improvement plan.
    improvementPlan :: Prelude.Maybe ChoiceContent
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
-- 'description', 'choice_description' - Undocumented member.
--
-- 'choiceId', 'choice_choiceId' - Undocumented member.
--
-- 'title', 'choice_title' - Undocumented member.
--
-- 'helpfulResource', 'choice_helpfulResource' - The choice level helpful resource.
--
-- 'additionalResources', 'choice_additionalResources' - The additional resources for a choice. A choice can have up to two
-- additional resources: one of type @HELPFUL_RESOURCE@, one of type
-- @IMPROVEMENT_PLAN@, or both.
--
-- 'improvementPlan', 'choice_improvementPlan' - The choice level improvement plan.
newChoice ::
  Choice
newChoice =
  Choice'
    { description = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      title = Prelude.Nothing,
      helpfulResource = Prelude.Nothing,
      additionalResources = Prelude.Nothing,
      improvementPlan = Prelude.Nothing
    }

-- | Undocumented member.
choice_description :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_description = Lens.lens (\Choice' {description} -> description) (\s@Choice' {} a -> s {description = a} :: Choice)

-- | Undocumented member.
choice_choiceId :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_choiceId = Lens.lens (\Choice' {choiceId} -> choiceId) (\s@Choice' {} a -> s {choiceId = a} :: Choice)

-- | Undocumented member.
choice_title :: Lens.Lens' Choice (Prelude.Maybe Prelude.Text)
choice_title = Lens.lens (\Choice' {title} -> title) (\s@Choice' {} a -> s {title = a} :: Choice)

-- | The choice level helpful resource.
choice_helpfulResource :: Lens.Lens' Choice (Prelude.Maybe ChoiceContent)
choice_helpfulResource = Lens.lens (\Choice' {helpfulResource} -> helpfulResource) (\s@Choice' {} a -> s {helpfulResource = a} :: Choice)

-- | The additional resources for a choice. A choice can have up to two
-- additional resources: one of type @HELPFUL_RESOURCE@, one of type
-- @IMPROVEMENT_PLAN@, or both.
choice_additionalResources :: Lens.Lens' Choice (Prelude.Maybe [AdditionalResources])
choice_additionalResources = Lens.lens (\Choice' {additionalResources} -> additionalResources) (\s@Choice' {} a -> s {additionalResources = a} :: Choice) Prelude.. Lens.mapping Lens.coerced

-- | The choice level improvement plan.
choice_improvementPlan :: Lens.Lens' Choice (Prelude.Maybe ChoiceContent)
choice_improvementPlan = Lens.lens (\Choice' {improvementPlan} -> improvementPlan) (\s@Choice' {} a -> s {improvementPlan = a} :: Choice)

instance Core.FromJSON Choice where
  parseJSON =
    Core.withObject
      "Choice"
      ( \x ->
          Choice'
            Prelude.<$> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ChoiceId")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "HelpfulResource")
            Prelude.<*> ( x Core..:? "AdditionalResources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ImprovementPlan")
      )

instance Prelude.Hashable Choice where
  hashWithSalt _salt Choice' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` helpfulResource
      `Prelude.hashWithSalt` additionalResources
      `Prelude.hashWithSalt` improvementPlan

instance Prelude.NFData Choice where
  rnf Choice' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf helpfulResource
      `Prelude.seq` Prelude.rnf additionalResources
      `Prelude.seq` Prelude.rnf improvementPlan
