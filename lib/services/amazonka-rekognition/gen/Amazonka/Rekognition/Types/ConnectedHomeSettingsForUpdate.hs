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
-- Module      : Amazonka.Rekognition.Types.ConnectedHomeSettingsForUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ConnectedHomeSettingsForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The label detection settings you want to use in your stream processor.
-- This includes the labels you want the stream processor to detect and the
-- minimum confidence level allowed to label objects.
--
-- /See:/ 'newConnectedHomeSettingsForUpdate' smart constructor.
data ConnectedHomeSettingsForUpdate = ConnectedHomeSettingsForUpdate'
  { -- | The minimum confidence required to label an object in the video.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | Specifies what you want to detect in the video, such as people,
    -- packages, or pets. The current valid labels you can include in this list
    -- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
    labels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectedHomeSettingsForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minConfidence', 'connectedHomeSettingsForUpdate_minConfidence' - The minimum confidence required to label an object in the video.
--
-- 'labels', 'connectedHomeSettingsForUpdate_labels' - Specifies what you want to detect in the video, such as people,
-- packages, or pets. The current valid labels you can include in this list
-- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
newConnectedHomeSettingsForUpdate ::
  ConnectedHomeSettingsForUpdate
newConnectedHomeSettingsForUpdate =
  ConnectedHomeSettingsForUpdate'
    { minConfidence =
        Prelude.Nothing,
      labels = Prelude.Nothing
    }

-- | The minimum confidence required to label an object in the video.
connectedHomeSettingsForUpdate_minConfidence :: Lens.Lens' ConnectedHomeSettingsForUpdate (Prelude.Maybe Prelude.Double)
connectedHomeSettingsForUpdate_minConfidence = Lens.lens (\ConnectedHomeSettingsForUpdate' {minConfidence} -> minConfidence) (\s@ConnectedHomeSettingsForUpdate' {} a -> s {minConfidence = a} :: ConnectedHomeSettingsForUpdate)

-- | Specifies what you want to detect in the video, such as people,
-- packages, or pets. The current valid labels you can include in this list
-- are: \"PERSON\", \"PET\", \"PACKAGE\", and \"ALL\".
connectedHomeSettingsForUpdate_labels :: Lens.Lens' ConnectedHomeSettingsForUpdate (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
connectedHomeSettingsForUpdate_labels = Lens.lens (\ConnectedHomeSettingsForUpdate' {labels} -> labels) (\s@ConnectedHomeSettingsForUpdate' {} a -> s {labels = a} :: ConnectedHomeSettingsForUpdate) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ConnectedHomeSettingsForUpdate
  where
  hashWithSalt
    _salt
    ConnectedHomeSettingsForUpdate' {..} =
      _salt `Prelude.hashWithSalt` minConfidence
        `Prelude.hashWithSalt` labels

instance
  Prelude.NFData
    ConnectedHomeSettingsForUpdate
  where
  rnf ConnectedHomeSettingsForUpdate' {..} =
    Prelude.rnf minConfidence
      `Prelude.seq` Prelude.rnf labels

instance Core.ToJSON ConnectedHomeSettingsForUpdate where
  toJSON ConnectedHomeSettingsForUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinConfidence" Core..=) Prelude.<$> minConfidence,
            ("Labels" Core..=) Prelude.<$> labels
          ]
      )
