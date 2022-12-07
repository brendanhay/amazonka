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
-- Module      : Amazonka.CloudTrail.Types.ResourceTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ResourceTag where

import Amazonka.CloudTrail.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource tag.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | Specifies the ARN of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags.
    tagsList :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resourceTag_resourceId' - Specifies the ARN of the resource.
--
-- 'tagsList', 'resourceTag_tagsList' - A list of tags.
newResourceTag ::
  ResourceTag
newResourceTag =
  ResourceTag'
    { resourceId = Prelude.Nothing,
      tagsList = Prelude.Nothing
    }

-- | Specifies the ARN of the resource.
resourceTag_resourceId :: Lens.Lens' ResourceTag (Prelude.Maybe Prelude.Text)
resourceTag_resourceId = Lens.lens (\ResourceTag' {resourceId} -> resourceId) (\s@ResourceTag' {} a -> s {resourceId = a} :: ResourceTag)

-- | A list of tags.
resourceTag_tagsList :: Lens.Lens' ResourceTag (Prelude.Maybe [Tag])
resourceTag_tagsList = Lens.lens (\ResourceTag' {tagsList} -> tagsList) (\s@ResourceTag' {} a -> s {tagsList = a} :: ResourceTag) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceTag where
  parseJSON =
    Data.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "TagsList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceTag where
  hashWithSalt _salt ResourceTag' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagsList

instance Prelude.NFData ResourceTag where
  rnf ResourceTag' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tagsList
