{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure describing the source of an action.
--
-- /See:/ 'newActionSource' smart constructor.
data ActionSource = ActionSource'
  { -- | The ID of the source.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the source.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source.
    sourceUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ActionSource
newActionSource pSourceUri_ =
  ActionSource'
    { sourceId = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceUri = pSourceUri_
    }

-- | The ID of the source.
actionSource_sourceId :: Lens.Lens' ActionSource (Prelude.Maybe Prelude.Text)
actionSource_sourceId = Lens.lens (\ActionSource' {sourceId} -> sourceId) (\s@ActionSource' {} a -> s {sourceId = a} :: ActionSource)

-- | The type of the source.
actionSource_sourceType :: Lens.Lens' ActionSource (Prelude.Maybe Prelude.Text)
actionSource_sourceType = Lens.lens (\ActionSource' {sourceType} -> sourceType) (\s@ActionSource' {} a -> s {sourceType = a} :: ActionSource)

-- | The URI of the source.
actionSource_sourceUri :: Lens.Lens' ActionSource Prelude.Text
actionSource_sourceUri = Lens.lens (\ActionSource' {sourceUri} -> sourceUri) (\s@ActionSource' {} a -> s {sourceUri = a} :: ActionSource)

instance Prelude.FromJSON ActionSource where
  parseJSON =
    Prelude.withObject
      "ActionSource"
      ( \x ->
          ActionSource'
            Prelude.<$> (x Prelude..:? "SourceId")
            Prelude.<*> (x Prelude..:? "SourceType")
            Prelude.<*> (x Prelude..: "SourceUri")
      )

instance Prelude.Hashable ActionSource

instance Prelude.NFData ActionSource

instance Prelude.ToJSON ActionSource where
  toJSON ActionSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SourceId" Prelude..=) Prelude.<$> sourceId,
            ("SourceType" Prelude..=) Prelude.<$> sourceType,
            Prelude.Just ("SourceUri" Prelude..= sourceUri)
          ]
      )
