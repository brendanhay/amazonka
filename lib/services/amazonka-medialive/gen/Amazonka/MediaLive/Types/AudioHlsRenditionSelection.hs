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
-- Module      : Amazonka.MediaLive.Types.AudioHlsRenditionSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioHlsRenditionSelection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Audio Hls Rendition Selection
--
-- /See:/ 'newAudioHlsRenditionSelection' smart constructor.
data AudioHlsRenditionSelection = AudioHlsRenditionSelection'
  { -- | Specifies the NAME in the #EXT-X-MEDIA tag of the target HLS audio
    -- rendition.
    name :: Prelude.Text,
    -- | Specifies the GROUP-ID in the #EXT-X-MEDIA tag of the target HLS audio
    -- rendition.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioHlsRenditionSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'audioHlsRenditionSelection_name' - Specifies the NAME in the #EXT-X-MEDIA tag of the target HLS audio
-- rendition.
--
-- 'groupId', 'audioHlsRenditionSelection_groupId' - Specifies the GROUP-ID in the #EXT-X-MEDIA tag of the target HLS audio
-- rendition.
newAudioHlsRenditionSelection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  AudioHlsRenditionSelection
newAudioHlsRenditionSelection pName_ pGroupId_ =
  AudioHlsRenditionSelection'
    { name = pName_,
      groupId = pGroupId_
    }

-- | Specifies the NAME in the #EXT-X-MEDIA tag of the target HLS audio
-- rendition.
audioHlsRenditionSelection_name :: Lens.Lens' AudioHlsRenditionSelection Prelude.Text
audioHlsRenditionSelection_name = Lens.lens (\AudioHlsRenditionSelection' {name} -> name) (\s@AudioHlsRenditionSelection' {} a -> s {name = a} :: AudioHlsRenditionSelection)

-- | Specifies the GROUP-ID in the #EXT-X-MEDIA tag of the target HLS audio
-- rendition.
audioHlsRenditionSelection_groupId :: Lens.Lens' AudioHlsRenditionSelection Prelude.Text
audioHlsRenditionSelection_groupId = Lens.lens (\AudioHlsRenditionSelection' {groupId} -> groupId) (\s@AudioHlsRenditionSelection' {} a -> s {groupId = a} :: AudioHlsRenditionSelection)

instance Core.FromJSON AudioHlsRenditionSelection where
  parseJSON =
    Core.withObject
      "AudioHlsRenditionSelection"
      ( \x ->
          AudioHlsRenditionSelection'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "groupId")
      )

instance Prelude.Hashable AudioHlsRenditionSelection where
  hashWithSalt _salt AudioHlsRenditionSelection' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData AudioHlsRenditionSelection where
  rnf AudioHlsRenditionSelection' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf groupId

instance Core.ToJSON AudioHlsRenditionSelection where
  toJSON AudioHlsRenditionSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("groupId" Core..= groupId)
          ]
      )
