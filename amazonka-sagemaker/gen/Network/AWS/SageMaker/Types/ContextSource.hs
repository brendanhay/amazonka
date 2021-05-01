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
-- Module      : Network.AWS.SageMaker.Types.ContextSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContextSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure describing the source of a context.
--
-- /See:/ 'newContextSource' smart constructor.
data ContextSource = ContextSource'
  { -- | The ID of the source.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the source.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source.
    sourceUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ContextSource
newContextSource pSourceUri_ =
  ContextSource'
    { sourceId = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceUri = pSourceUri_
    }

-- | The ID of the source.
contextSource_sourceId :: Lens.Lens' ContextSource (Prelude.Maybe Prelude.Text)
contextSource_sourceId = Lens.lens (\ContextSource' {sourceId} -> sourceId) (\s@ContextSource' {} a -> s {sourceId = a} :: ContextSource)

-- | The type of the source.
contextSource_sourceType :: Lens.Lens' ContextSource (Prelude.Maybe Prelude.Text)
contextSource_sourceType = Lens.lens (\ContextSource' {sourceType} -> sourceType) (\s@ContextSource' {} a -> s {sourceType = a} :: ContextSource)

-- | The URI of the source.
contextSource_sourceUri :: Lens.Lens' ContextSource Prelude.Text
contextSource_sourceUri = Lens.lens (\ContextSource' {sourceUri} -> sourceUri) (\s@ContextSource' {} a -> s {sourceUri = a} :: ContextSource)

instance Prelude.FromJSON ContextSource where
  parseJSON =
    Prelude.withObject
      "ContextSource"
      ( \x ->
          ContextSource'
            Prelude.<$> (x Prelude..:? "SourceId")
            Prelude.<*> (x Prelude..:? "SourceType")
            Prelude.<*> (x Prelude..: "SourceUri")
      )

instance Prelude.Hashable ContextSource

instance Prelude.NFData ContextSource

instance Prelude.ToJSON ContextSource where
  toJSON ContextSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SourceId" Prelude..=) Prelude.<$> sourceId,
            ("SourceType" Prelude..=) Prelude.<$> sourceType,
            Prelude.Just ("SourceUri" Prelude..= sourceUri)
          ]
      )
