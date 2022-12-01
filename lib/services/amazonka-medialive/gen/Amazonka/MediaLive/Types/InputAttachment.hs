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
-- Module      : Amazonka.MediaLive.Types.InputAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.AutomaticInputFailoverSettings
import Amazonka.MediaLive.Types.InputSettings
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'newInputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { -- | The ID of the input
    inputId :: Prelude.Maybe Prelude.Text,
    -- | User-specified settings for defining what the conditions are for
    -- declaring the input unhealthy and failing over to a different input.
    automaticInputFailoverSettings :: Prelude.Maybe AutomaticInputFailoverSettings,
    -- | User-specified name for the attachment. This is required if the user
    -- wants to use this input in an input switch action.
    inputAttachmentName :: Prelude.Maybe Prelude.Text,
    -- | Settings of an input (caption selector, etc.)
    inputSettings :: Prelude.Maybe InputSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputId', 'inputAttachment_inputId' - The ID of the input
--
-- 'automaticInputFailoverSettings', 'inputAttachment_automaticInputFailoverSettings' - User-specified settings for defining what the conditions are for
-- declaring the input unhealthy and failing over to a different input.
--
-- 'inputAttachmentName', 'inputAttachment_inputAttachmentName' - User-specified name for the attachment. This is required if the user
-- wants to use this input in an input switch action.
--
-- 'inputSettings', 'inputAttachment_inputSettings' - Settings of an input (caption selector, etc.)
newInputAttachment ::
  InputAttachment
newInputAttachment =
  InputAttachment'
    { inputId = Prelude.Nothing,
      automaticInputFailoverSettings = Prelude.Nothing,
      inputAttachmentName = Prelude.Nothing,
      inputSettings = Prelude.Nothing
    }

-- | The ID of the input
inputAttachment_inputId :: Lens.Lens' InputAttachment (Prelude.Maybe Prelude.Text)
inputAttachment_inputId = Lens.lens (\InputAttachment' {inputId} -> inputId) (\s@InputAttachment' {} a -> s {inputId = a} :: InputAttachment)

-- | User-specified settings for defining what the conditions are for
-- declaring the input unhealthy and failing over to a different input.
inputAttachment_automaticInputFailoverSettings :: Lens.Lens' InputAttachment (Prelude.Maybe AutomaticInputFailoverSettings)
inputAttachment_automaticInputFailoverSettings = Lens.lens (\InputAttachment' {automaticInputFailoverSettings} -> automaticInputFailoverSettings) (\s@InputAttachment' {} a -> s {automaticInputFailoverSettings = a} :: InputAttachment)

-- | User-specified name for the attachment. This is required if the user
-- wants to use this input in an input switch action.
inputAttachment_inputAttachmentName :: Lens.Lens' InputAttachment (Prelude.Maybe Prelude.Text)
inputAttachment_inputAttachmentName = Lens.lens (\InputAttachment' {inputAttachmentName} -> inputAttachmentName) (\s@InputAttachment' {} a -> s {inputAttachmentName = a} :: InputAttachment)

-- | Settings of an input (caption selector, etc.)
inputAttachment_inputSettings :: Lens.Lens' InputAttachment (Prelude.Maybe InputSettings)
inputAttachment_inputSettings = Lens.lens (\InputAttachment' {inputSettings} -> inputSettings) (\s@InputAttachment' {} a -> s {inputSettings = a} :: InputAttachment)

instance Core.FromJSON InputAttachment where
  parseJSON =
    Core.withObject
      "InputAttachment"
      ( \x ->
          InputAttachment'
            Prelude.<$> (x Core..:? "inputId")
            Prelude.<*> (x Core..:? "automaticInputFailoverSettings")
            Prelude.<*> (x Core..:? "inputAttachmentName")
            Prelude.<*> (x Core..:? "inputSettings")
      )

instance Prelude.Hashable InputAttachment where
  hashWithSalt _salt InputAttachment' {..} =
    _salt `Prelude.hashWithSalt` inputId
      `Prelude.hashWithSalt` automaticInputFailoverSettings
      `Prelude.hashWithSalt` inputAttachmentName
      `Prelude.hashWithSalt` inputSettings

instance Prelude.NFData InputAttachment where
  rnf InputAttachment' {..} =
    Prelude.rnf inputId
      `Prelude.seq` Prelude.rnf automaticInputFailoverSettings
      `Prelude.seq` Prelude.rnf inputAttachmentName
      `Prelude.seq` Prelude.rnf inputSettings

instance Core.ToJSON InputAttachment where
  toJSON InputAttachment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("inputId" Core..=) Prelude.<$> inputId,
            ("automaticInputFailoverSettings" Core..=)
              Prelude.<$> automaticInputFailoverSettings,
            ("inputAttachmentName" Core..=)
              Prelude.<$> inputAttachmentName,
            ("inputSettings" Core..=) Prelude.<$> inputSettings
          ]
      )
