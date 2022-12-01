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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorSettingsForUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorSettingsForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ConnectedHomeSettingsForUpdate

-- | The stream processor settings that you want to update. @ConnectedHome@
-- settings can be updated to detect different labels with a different
-- minimum confidence.
--
-- /See:/ 'newStreamProcessorSettingsForUpdate' smart constructor.
data StreamProcessorSettingsForUpdate = StreamProcessorSettingsForUpdate'
  { -- | The label detection settings you want to use for your stream processor.
    connectedHomeForUpdate :: Prelude.Maybe ConnectedHomeSettingsForUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorSettingsForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectedHomeForUpdate', 'streamProcessorSettingsForUpdate_connectedHomeForUpdate' - The label detection settings you want to use for your stream processor.
newStreamProcessorSettingsForUpdate ::
  StreamProcessorSettingsForUpdate
newStreamProcessorSettingsForUpdate =
  StreamProcessorSettingsForUpdate'
    { connectedHomeForUpdate =
        Prelude.Nothing
    }

-- | The label detection settings you want to use for your stream processor.
streamProcessorSettingsForUpdate_connectedHomeForUpdate :: Lens.Lens' StreamProcessorSettingsForUpdate (Prelude.Maybe ConnectedHomeSettingsForUpdate)
streamProcessorSettingsForUpdate_connectedHomeForUpdate = Lens.lens (\StreamProcessorSettingsForUpdate' {connectedHomeForUpdate} -> connectedHomeForUpdate) (\s@StreamProcessorSettingsForUpdate' {} a -> s {connectedHomeForUpdate = a} :: StreamProcessorSettingsForUpdate)

instance
  Prelude.Hashable
    StreamProcessorSettingsForUpdate
  where
  hashWithSalt
    _salt
    StreamProcessorSettingsForUpdate' {..} =
      _salt `Prelude.hashWithSalt` connectedHomeForUpdate

instance
  Prelude.NFData
    StreamProcessorSettingsForUpdate
  where
  rnf StreamProcessorSettingsForUpdate' {..} =
    Prelude.rnf connectedHomeForUpdate

instance Core.ToJSON StreamProcessorSettingsForUpdate where
  toJSON StreamProcessorSettingsForUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConnectedHomeForUpdate" Core..=)
              Prelude.<$> connectedHomeForUpdate
          ]
      )
