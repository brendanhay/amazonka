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
-- Module      : Network.AWS.MediaConvert.Types.ResourceTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ResourceTags where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental
-- MediaConvert resource.
--
-- /See:/ 'newResourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Core.Maybe Core.Text,
    -- | The tags for the resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceTags_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tags', 'resourceTags_tags' - The tags for the resource.
newResourceTags ::
  ResourceTags
newResourceTags =
  ResourceTags'
    { arn = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
resourceTags_arn :: Lens.Lens' ResourceTags (Core.Maybe Core.Text)
resourceTags_arn = Lens.lens (\ResourceTags' {arn} -> arn) (\s@ResourceTags' {} a -> s {arn = a} :: ResourceTags)

-- | The tags for the resource.
resourceTags_tags :: Lens.Lens' ResourceTags (Core.Maybe (Core.HashMap Core.Text Core.Text))
resourceTags_tags = Lens.lens (\ResourceTags' {tags} -> tags) (\s@ResourceTags' {} a -> s {tags = a} :: ResourceTags) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ResourceTags where
  parseJSON =
    Core.withObject
      "ResourceTags"
      ( \x ->
          ResourceTags'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
      )

instance Core.Hashable ResourceTags

instance Core.NFData ResourceTags
