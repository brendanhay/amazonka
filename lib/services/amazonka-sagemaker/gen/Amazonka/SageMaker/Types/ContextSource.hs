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
-- Module      : Amazonka.SageMaker.Types.ContextSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContextSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure describing the source of a context.
--
-- /See:/ 'newContextSource' smart constructor.
data ContextSource = ContextSource'
  { -- | The type of the source.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source.
    sourceUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContextSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'contextSource_sourceType' - The type of the source.
--
-- 'sourceId', 'contextSource_sourceId' - The ID of the source.
--
-- 'sourceUri', 'contextSource_sourceUri' - The URI of the source.
newContextSource ::
  -- | 'sourceUri'
  Prelude.Text ->
  ContextSource
newContextSource pSourceUri_ =
  ContextSource'
    { sourceType = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      sourceUri = pSourceUri_
    }

-- | The type of the source.
contextSource_sourceType :: Lens.Lens' ContextSource (Prelude.Maybe Prelude.Text)
contextSource_sourceType = Lens.lens (\ContextSource' {sourceType} -> sourceType) (\s@ContextSource' {} a -> s {sourceType = a} :: ContextSource)

-- | The ID of the source.
contextSource_sourceId :: Lens.Lens' ContextSource (Prelude.Maybe Prelude.Text)
contextSource_sourceId = Lens.lens (\ContextSource' {sourceId} -> sourceId) (\s@ContextSource' {} a -> s {sourceId = a} :: ContextSource)

-- | The URI of the source.
contextSource_sourceUri :: Lens.Lens' ContextSource Prelude.Text
contextSource_sourceUri = Lens.lens (\ContextSource' {sourceUri} -> sourceUri) (\s@ContextSource' {} a -> s {sourceUri = a} :: ContextSource)

instance Core.FromJSON ContextSource where
  parseJSON =
    Core.withObject
      "ContextSource"
      ( \x ->
          ContextSource'
            Prelude.<$> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SourceId")
            Prelude.<*> (x Core..: "SourceUri")
      )

instance Prelude.Hashable ContextSource where
  hashWithSalt salt' ContextSource' {..} =
    salt' `Prelude.hashWithSalt` sourceUri
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData ContextSource where
  rnf ContextSource' {..} =
    Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceUri
      `Prelude.seq` Prelude.rnf sourceId

instance Core.ToJSON ContextSource where
  toJSON ContextSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceType" Core..=) Prelude.<$> sourceType,
            ("SourceId" Core..=) Prelude.<$> sourceId,
            Prelude.Just ("SourceUri" Core..= sourceUri)
          ]
      )
