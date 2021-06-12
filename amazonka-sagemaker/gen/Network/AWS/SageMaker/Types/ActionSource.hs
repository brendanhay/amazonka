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
-- Module      : Network.AWS.SageMaker.Types.ActionSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ActionSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure describing the source of an action.
--
-- /See:/ 'newActionSource' smart constructor.
data ActionSource = ActionSource'
  { -- | The ID of the source.
    sourceId :: Core.Maybe Core.Text,
    -- | The type of the source.
    sourceType :: Core.Maybe Core.Text,
    -- | The URI of the source.
    sourceUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'actionSource_sourceId' - The ID of the source.
--
-- 'sourceType', 'actionSource_sourceType' - The type of the source.
--
-- 'sourceUri', 'actionSource_sourceUri' - The URI of the source.
newActionSource ::
  -- | 'sourceUri'
  Core.Text ->
  ActionSource
newActionSource pSourceUri_ =
  ActionSource'
    { sourceId = Core.Nothing,
      sourceType = Core.Nothing,
      sourceUri = pSourceUri_
    }

-- | The ID of the source.
actionSource_sourceId :: Lens.Lens' ActionSource (Core.Maybe Core.Text)
actionSource_sourceId = Lens.lens (\ActionSource' {sourceId} -> sourceId) (\s@ActionSource' {} a -> s {sourceId = a} :: ActionSource)

-- | The type of the source.
actionSource_sourceType :: Lens.Lens' ActionSource (Core.Maybe Core.Text)
actionSource_sourceType = Lens.lens (\ActionSource' {sourceType} -> sourceType) (\s@ActionSource' {} a -> s {sourceType = a} :: ActionSource)

-- | The URI of the source.
actionSource_sourceUri :: Lens.Lens' ActionSource Core.Text
actionSource_sourceUri = Lens.lens (\ActionSource' {sourceUri} -> sourceUri) (\s@ActionSource' {} a -> s {sourceUri = a} :: ActionSource)

instance Core.FromJSON ActionSource where
  parseJSON =
    Core.withObject
      "ActionSource"
      ( \x ->
          ActionSource'
            Core.<$> (x Core..:? "SourceId")
            Core.<*> (x Core..:? "SourceType")
            Core.<*> (x Core..: "SourceUri")
      )

instance Core.Hashable ActionSource

instance Core.NFData ActionSource

instance Core.ToJSON ActionSource where
  toJSON ActionSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourceId" Core..=) Core.<$> sourceId,
            ("SourceType" Core..=) Core.<$> sourceType,
            Core.Just ("SourceUri" Core..= sourceUri)
          ]
      )
