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
-- Module      : Amazonka.Panorama.Types.JobResourceTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.JobResourceTags where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.JobResourceType
import qualified Amazonka.Prelude as Prelude

-- | Tags for a job.
--
-- /See:/ 'newJobResourceTags' smart constructor.
data JobResourceTags = JobResourceTags'
  { -- | The job\'s type.
    resourceType :: JobResourceType,
    -- | The job\'s tags.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobResourceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'jobResourceTags_resourceType' - The job\'s type.
--
-- 'tags', 'jobResourceTags_tags' - The job\'s tags.
newJobResourceTags ::
  -- | 'resourceType'
  JobResourceType ->
  JobResourceTags
newJobResourceTags pResourceType_ =
  JobResourceTags'
    { resourceType = pResourceType_,
      tags = Prelude.mempty
    }

-- | The job\'s type.
jobResourceTags_resourceType :: Lens.Lens' JobResourceTags JobResourceType
jobResourceTags_resourceType = Lens.lens (\JobResourceTags' {resourceType} -> resourceType) (\s@JobResourceTags' {} a -> s {resourceType = a} :: JobResourceTags)

-- | The job\'s tags.
jobResourceTags_tags :: Lens.Lens' JobResourceTags (Prelude.HashMap Prelude.Text Prelude.Text)
jobResourceTags_tags = Lens.lens (\JobResourceTags' {tags} -> tags) (\s@JobResourceTags' {} a -> s {tags = a} :: JobResourceTags) Prelude.. Lens.coerced

instance Data.FromJSON JobResourceTags where
  parseJSON =
    Data.withObject
      "JobResourceTags"
      ( \x ->
          JobResourceTags'
            Prelude.<$> (x Data..: "ResourceType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobResourceTags where
  hashWithSalt _salt JobResourceTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData JobResourceTags where
  rnf JobResourceTags' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON JobResourceTags where
  toJSON JobResourceTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )
