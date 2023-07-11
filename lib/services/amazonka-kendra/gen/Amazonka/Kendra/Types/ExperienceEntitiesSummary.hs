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
-- Module      : Amazonka.Kendra.Types.ExperienceEntitiesSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ExperienceEntitiesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.EntityDisplayData
import Amazonka.Kendra.Types.EntityType
import qualified Amazonka.Prelude as Prelude

-- | Summary information for users or groups in your IAM Identity Center
-- identity source with granted access to your Amazon Kendra experience.
-- You can create an Amazon Kendra experience such as a search application.
-- For more information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
--
-- /See:/ 'newExperienceEntitiesSummary' smart constructor.
data ExperienceEntitiesSummary = ExperienceEntitiesSummary'
  { -- | Information about the user entity.
    displayData :: Prelude.Maybe EntityDisplayData,
    -- | The identifier of a user or group in your IAM Identity Center identity
    -- source. For example, a user ID could be an email.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | Shows the type as @User@ or @Group@.
    entityType :: Prelude.Maybe EntityType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperienceEntitiesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayData', 'experienceEntitiesSummary_displayData' - Information about the user entity.
--
-- 'entityId', 'experienceEntitiesSummary_entityId' - The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
--
-- 'entityType', 'experienceEntitiesSummary_entityType' - Shows the type as @User@ or @Group@.
newExperienceEntitiesSummary ::
  ExperienceEntitiesSummary
newExperienceEntitiesSummary =
  ExperienceEntitiesSummary'
    { displayData =
        Prelude.Nothing,
      entityId = Prelude.Nothing,
      entityType = Prelude.Nothing
    }

-- | Information about the user entity.
experienceEntitiesSummary_displayData :: Lens.Lens' ExperienceEntitiesSummary (Prelude.Maybe EntityDisplayData)
experienceEntitiesSummary_displayData = Lens.lens (\ExperienceEntitiesSummary' {displayData} -> displayData) (\s@ExperienceEntitiesSummary' {} a -> s {displayData = a} :: ExperienceEntitiesSummary)

-- | The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
experienceEntitiesSummary_entityId :: Lens.Lens' ExperienceEntitiesSummary (Prelude.Maybe Prelude.Text)
experienceEntitiesSummary_entityId = Lens.lens (\ExperienceEntitiesSummary' {entityId} -> entityId) (\s@ExperienceEntitiesSummary' {} a -> s {entityId = a} :: ExperienceEntitiesSummary)

-- | Shows the type as @User@ or @Group@.
experienceEntitiesSummary_entityType :: Lens.Lens' ExperienceEntitiesSummary (Prelude.Maybe EntityType)
experienceEntitiesSummary_entityType = Lens.lens (\ExperienceEntitiesSummary' {entityType} -> entityType) (\s@ExperienceEntitiesSummary' {} a -> s {entityType = a} :: ExperienceEntitiesSummary)

instance Data.FromJSON ExperienceEntitiesSummary where
  parseJSON =
    Data.withObject
      "ExperienceEntitiesSummary"
      ( \x ->
          ExperienceEntitiesSummary'
            Prelude.<$> (x Data..:? "DisplayData")
            Prelude.<*> (x Data..:? "EntityId")
            Prelude.<*> (x Data..:? "EntityType")
      )

instance Prelude.Hashable ExperienceEntitiesSummary where
  hashWithSalt _salt ExperienceEntitiesSummary' {..} =
    _salt
      `Prelude.hashWithSalt` displayData
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` entityType

instance Prelude.NFData ExperienceEntitiesSummary where
  rnf ExperienceEntitiesSummary' {..} =
    Prelude.rnf displayData
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf entityType
