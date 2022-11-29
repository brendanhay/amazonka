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
-- Module      : Amazonka.Kendra.Types.ExperiencesSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ExperiencesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.ExperienceEndpoint
import Amazonka.Kendra.Types.ExperienceStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for your Amazon Kendra experience. You can create an
-- Amazon Kendra experience such as a search application. For more
-- information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
--
-- /See:/ 'newExperiencesSummary' smart constructor.
data ExperiencesSummary = ExperiencesSummary'
  { -- | The name of your Amazon Kendra experience.
    name :: Prelude.Maybe Prelude.Text,
    -- | The processing status of your Amazon Kendra experience.
    status :: Prelude.Maybe ExperienceStatus,
    -- | The endpoint URLs for your Amazon Kendra experiences. The URLs are
    -- unique and fully hosted by Amazon Web Services.
    endpoints :: Prelude.Maybe (Prelude.NonEmpty ExperienceEndpoint),
    -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date-time your Amazon Kendra experience was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperiencesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'experiencesSummary_name' - The name of your Amazon Kendra experience.
--
-- 'status', 'experiencesSummary_status' - The processing status of your Amazon Kendra experience.
--
-- 'endpoints', 'experiencesSummary_endpoints' - The endpoint URLs for your Amazon Kendra experiences. The URLs are
-- unique and fully hosted by Amazon Web Services.
--
-- 'id', 'experiencesSummary_id' - The identifier of your Amazon Kendra experience.
--
-- 'createdAt', 'experiencesSummary_createdAt' - The date-time your Amazon Kendra experience was created.
newExperiencesSummary ::
  ExperiencesSummary
newExperiencesSummary =
  ExperiencesSummary'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The name of your Amazon Kendra experience.
experiencesSummary_name :: Lens.Lens' ExperiencesSummary (Prelude.Maybe Prelude.Text)
experiencesSummary_name = Lens.lens (\ExperiencesSummary' {name} -> name) (\s@ExperiencesSummary' {} a -> s {name = a} :: ExperiencesSummary)

-- | The processing status of your Amazon Kendra experience.
experiencesSummary_status :: Lens.Lens' ExperiencesSummary (Prelude.Maybe ExperienceStatus)
experiencesSummary_status = Lens.lens (\ExperiencesSummary' {status} -> status) (\s@ExperiencesSummary' {} a -> s {status = a} :: ExperiencesSummary)

-- | The endpoint URLs for your Amazon Kendra experiences. The URLs are
-- unique and fully hosted by Amazon Web Services.
experiencesSummary_endpoints :: Lens.Lens' ExperiencesSummary (Prelude.Maybe (Prelude.NonEmpty ExperienceEndpoint))
experiencesSummary_endpoints = Lens.lens (\ExperiencesSummary' {endpoints} -> endpoints) (\s@ExperiencesSummary' {} a -> s {endpoints = a} :: ExperiencesSummary) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of your Amazon Kendra experience.
experiencesSummary_id :: Lens.Lens' ExperiencesSummary (Prelude.Maybe Prelude.Text)
experiencesSummary_id = Lens.lens (\ExperiencesSummary' {id} -> id) (\s@ExperiencesSummary' {} a -> s {id = a} :: ExperiencesSummary)

-- | The date-time your Amazon Kendra experience was created.
experiencesSummary_createdAt :: Lens.Lens' ExperiencesSummary (Prelude.Maybe Prelude.UTCTime)
experiencesSummary_createdAt = Lens.lens (\ExperiencesSummary' {createdAt} -> createdAt) (\s@ExperiencesSummary' {} a -> s {createdAt = a} :: ExperiencesSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ExperiencesSummary where
  parseJSON =
    Core.withObject
      "ExperiencesSummary"
      ( \x ->
          ExperiencesSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Endpoints")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable ExperiencesSummary where
  hashWithSalt _salt ExperiencesSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData ExperiencesSummary where
  rnf ExperiencesSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
