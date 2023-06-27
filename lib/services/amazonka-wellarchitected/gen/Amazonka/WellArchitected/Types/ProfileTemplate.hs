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
-- Module      : Amazonka.WellArchitected.Types.ProfileTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ProfileTemplateQuestion

-- | The profile template.
--
-- /See:/ 'newProfileTemplate' smart constructor.
data ProfileTemplate = ProfileTemplate'
  { createdAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the profile template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | Profile template questions.
    templateQuestions :: Prelude.Maybe [ProfileTemplateQuestion],
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'profileTemplate_createdAt' - Undocumented member.
--
-- 'templateName', 'profileTemplate_templateName' - The name of the profile template.
--
-- 'templateQuestions', 'profileTemplate_templateQuestions' - Profile template questions.
--
-- 'updatedAt', 'profileTemplate_updatedAt' - Undocumented member.
newProfileTemplate ::
  ProfileTemplate
newProfileTemplate =
  ProfileTemplate'
    { createdAt = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateQuestions = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
profileTemplate_createdAt :: Lens.Lens' ProfileTemplate (Prelude.Maybe Prelude.UTCTime)
profileTemplate_createdAt = Lens.lens (\ProfileTemplate' {createdAt} -> createdAt) (\s@ProfileTemplate' {} a -> s {createdAt = a} :: ProfileTemplate) Prelude.. Lens.mapping Data._Time

-- | The name of the profile template.
profileTemplate_templateName :: Lens.Lens' ProfileTemplate (Prelude.Maybe Prelude.Text)
profileTemplate_templateName = Lens.lens (\ProfileTemplate' {templateName} -> templateName) (\s@ProfileTemplate' {} a -> s {templateName = a} :: ProfileTemplate)

-- | Profile template questions.
profileTemplate_templateQuestions :: Lens.Lens' ProfileTemplate (Prelude.Maybe [ProfileTemplateQuestion])
profileTemplate_templateQuestions = Lens.lens (\ProfileTemplate' {templateQuestions} -> templateQuestions) (\s@ProfileTemplate' {} a -> s {templateQuestions = a} :: ProfileTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
profileTemplate_updatedAt :: Lens.Lens' ProfileTemplate (Prelude.Maybe Prelude.UTCTime)
profileTemplate_updatedAt = Lens.lens (\ProfileTemplate' {updatedAt} -> updatedAt) (\s@ProfileTemplate' {} a -> s {updatedAt = a} :: ProfileTemplate) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProfileTemplate where
  parseJSON =
    Data.withObject
      "ProfileTemplate"
      ( \x ->
          ProfileTemplate'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "TemplateName")
            Prelude.<*> ( x
                            Data..:? "TemplateQuestions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable ProfileTemplate where
  hashWithSalt _salt ProfileTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateQuestions
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ProfileTemplate where
  rnf ProfileTemplate' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateQuestions
      `Prelude.seq` Prelude.rnf updatedAt
