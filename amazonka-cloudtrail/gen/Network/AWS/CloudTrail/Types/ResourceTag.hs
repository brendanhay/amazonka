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
-- Module      : Network.AWS.CloudTrail.Types.ResourceTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ResourceTag where

import Network.AWS.CloudTrail.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A resource tag.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | Specifies the ARN of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | A list of tags.
    tagsList :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { resourceId = Core.Nothing,
      tagsList = Core.Nothing
    }

-- | Specifies the ARN of the resource.
resourceTag_resourceId :: Lens.Lens' ResourceTag (Core.Maybe Core.Text)
resourceTag_resourceId = Lens.lens (\ResourceTag' {resourceId} -> resourceId) (\s@ResourceTag' {} a -> s {resourceId = a} :: ResourceTag)

-- | A list of tags.
resourceTag_tagsList :: Lens.Lens' ResourceTag (Core.Maybe [Tag])
resourceTag_tagsList = Lens.lens (\ResourceTag' {tagsList} -> tagsList) (\s@ResourceTag' {} a -> s {tagsList = a} :: ResourceTag) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ResourceTag where
  parseJSON =
    Core.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "TagsList" Core..!= Core.mempty)
      )

instance Core.Hashable ResourceTag

instance Core.NFData ResourceTag
