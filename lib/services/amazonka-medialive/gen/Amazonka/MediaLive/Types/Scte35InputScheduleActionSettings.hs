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
-- Module      : Amazonka.MediaLive.Types.Scte35InputScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35InputScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Scte35InputMode
import qualified Amazonka.Prelude as Prelude

-- | Settings for the \"scte35 input\" action
--
-- /See:/ 'newScte35InputScheduleActionSettings' smart constructor.
data Scte35InputScheduleActionSettings = Scte35InputScheduleActionSettings'
  { -- | In fixed mode, enter the name of the input attachment that you want to
    -- use as a SCTE-35 input. (Don\'t enter the ID of the input.)\"
    inputAttachmentNameReference :: Prelude.Maybe Prelude.Text,
    -- | Whether the SCTE-35 input should be the active input or a fixed input.
    mode :: Scte35InputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte35InputScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputAttachmentNameReference', 'scte35InputScheduleActionSettings_inputAttachmentNameReference' - In fixed mode, enter the name of the input attachment that you want to
-- use as a SCTE-35 input. (Don\'t enter the ID of the input.)\"
--
-- 'mode', 'scte35InputScheduleActionSettings_mode' - Whether the SCTE-35 input should be the active input or a fixed input.
newScte35InputScheduleActionSettings ::
  -- | 'mode'
  Scte35InputMode ->
  Scte35InputScheduleActionSettings
newScte35InputScheduleActionSettings pMode_ =
  Scte35InputScheduleActionSettings'
    { inputAttachmentNameReference =
        Prelude.Nothing,
      mode = pMode_
    }

-- | In fixed mode, enter the name of the input attachment that you want to
-- use as a SCTE-35 input. (Don\'t enter the ID of the input.)\"
scte35InputScheduleActionSettings_inputAttachmentNameReference :: Lens.Lens' Scte35InputScheduleActionSettings (Prelude.Maybe Prelude.Text)
scte35InputScheduleActionSettings_inputAttachmentNameReference = Lens.lens (\Scte35InputScheduleActionSettings' {inputAttachmentNameReference} -> inputAttachmentNameReference) (\s@Scte35InputScheduleActionSettings' {} a -> s {inputAttachmentNameReference = a} :: Scte35InputScheduleActionSettings)

-- | Whether the SCTE-35 input should be the active input or a fixed input.
scte35InputScheduleActionSettings_mode :: Lens.Lens' Scte35InputScheduleActionSettings Scte35InputMode
scte35InputScheduleActionSettings_mode = Lens.lens (\Scte35InputScheduleActionSettings' {mode} -> mode) (\s@Scte35InputScheduleActionSettings' {} a -> s {mode = a} :: Scte35InputScheduleActionSettings)

instance
  Data.FromJSON
    Scte35InputScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "Scte35InputScheduleActionSettings"
      ( \x ->
          Scte35InputScheduleActionSettings'
            Prelude.<$> (x Data..:? "inputAttachmentNameReference")
            Prelude.<*> (x Data..: "mode")
      )

instance
  Prelude.Hashable
    Scte35InputScheduleActionSettings
  where
  hashWithSalt
    _salt
    Scte35InputScheduleActionSettings' {..} =
      _salt
        `Prelude.hashWithSalt` inputAttachmentNameReference
        `Prelude.hashWithSalt` mode

instance
  Prelude.NFData
    Scte35InputScheduleActionSettings
  where
  rnf Scte35InputScheduleActionSettings' {..} =
    Prelude.rnf inputAttachmentNameReference
      `Prelude.seq` Prelude.rnf mode

instance
  Data.ToJSON
    Scte35InputScheduleActionSettings
  where
  toJSON Scte35InputScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputAttachmentNameReference" Data..=)
              Prelude.<$> inputAttachmentNameReference,
            Prelude.Just ("mode" Data..= mode)
          ]
      )
