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
-- Module      : Network.AWS.SageMaker.Types.ContextSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContextSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure describing the source of a context.
--
-- /See:/ 'newContextSource' smart constructor.
data ContextSource = ContextSource'
  { -- | The ID of the source.
    sourceId :: Core.Maybe Core.Text,
    -- | The type of the source.
    sourceType :: Core.Maybe Core.Text,
    -- | The URI of the source.
    sourceUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContextSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'contextSource_sourceId' - The ID of the source.
--
-- 'sourceType', 'contextSource_sourceType' - The type of the source.
--
-- 'sourceUri', 'contextSource_sourceUri' - The URI of the source.
newContextSource ::
  -- | 'sourceUri'
  Core.Text ->
  ContextSource
newContextSource pSourceUri_ =
  ContextSource'
    { sourceId = Core.Nothing,
      sourceType = Core.Nothing,
      sourceUri = pSourceUri_
    }

-- | The ID of the source.
contextSource_sourceId :: Lens.Lens' ContextSource (Core.Maybe Core.Text)
contextSource_sourceId = Lens.lens (\ContextSource' {sourceId} -> sourceId) (\s@ContextSource' {} a -> s {sourceId = a} :: ContextSource)

-- | The type of the source.
contextSource_sourceType :: Lens.Lens' ContextSource (Core.Maybe Core.Text)
contextSource_sourceType = Lens.lens (\ContextSource' {sourceType} -> sourceType) (\s@ContextSource' {} a -> s {sourceType = a} :: ContextSource)

-- | The URI of the source.
contextSource_sourceUri :: Lens.Lens' ContextSource Core.Text
contextSource_sourceUri = Lens.lens (\ContextSource' {sourceUri} -> sourceUri) (\s@ContextSource' {} a -> s {sourceUri = a} :: ContextSource)

instance Core.FromJSON ContextSource where
  parseJSON =
    Core.withObject
      "ContextSource"
      ( \x ->
          ContextSource'
            Core.<$> (x Core..:? "SourceId")
            Core.<*> (x Core..:? "SourceType")
            Core.<*> (x Core..: "SourceUri")
      )

instance Core.Hashable ContextSource

instance Core.NFData ContextSource

instance Core.ToJSON ContextSource where
  toJSON ContextSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourceId" Core..=) Core.<$> sourceId,
            ("SourceType" Core..=) Core.<$> sourceType,
            Core.Just ("SourceUri" Core..= sourceUri)
          ]
      )
