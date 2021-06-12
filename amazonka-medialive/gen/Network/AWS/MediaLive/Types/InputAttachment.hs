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
-- Module      : Network.AWS.MediaLive.Types.InputAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputAttachment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.InputSettings

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'newInputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { -- | Settings of an input (caption selector, etc.)
    inputSettings :: Core.Maybe InputSettings,
    -- | The ID of the input
    inputId :: Core.Maybe Core.Text,
    -- | User-specified name for the attachment. This is required if the user
    -- wants to use this input in an input switch action.
    inputAttachmentName :: Core.Maybe Core.Text,
    -- | User-specified settings for defining what the conditions are for
    -- declaring the input unhealthy and failing over to a different input.
    automaticInputFailoverSettings :: Core.Maybe AutomaticInputFailoverSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSettings', 'inputAttachment_inputSettings' - Settings of an input (caption selector, etc.)
--
-- 'inputId', 'inputAttachment_inputId' - The ID of the input
--
-- 'inputAttachmentName', 'inputAttachment_inputAttachmentName' - User-specified name for the attachment. This is required if the user
-- wants to use this input in an input switch action.
--
-- 'automaticInputFailoverSettings', 'inputAttachment_automaticInputFailoverSettings' - User-specified settings for defining what the conditions are for
-- declaring the input unhealthy and failing over to a different input.
newInputAttachment ::
  InputAttachment
newInputAttachment =
  InputAttachment'
    { inputSettings = Core.Nothing,
      inputId = Core.Nothing,
      inputAttachmentName = Core.Nothing,
      automaticInputFailoverSettings = Core.Nothing
    }

-- | Settings of an input (caption selector, etc.)
inputAttachment_inputSettings :: Lens.Lens' InputAttachment (Core.Maybe InputSettings)
inputAttachment_inputSettings = Lens.lens (\InputAttachment' {inputSettings} -> inputSettings) (\s@InputAttachment' {} a -> s {inputSettings = a} :: InputAttachment)

-- | The ID of the input
inputAttachment_inputId :: Lens.Lens' InputAttachment (Core.Maybe Core.Text)
inputAttachment_inputId = Lens.lens (\InputAttachment' {inputId} -> inputId) (\s@InputAttachment' {} a -> s {inputId = a} :: InputAttachment)

-- | User-specified name for the attachment. This is required if the user
-- wants to use this input in an input switch action.
inputAttachment_inputAttachmentName :: Lens.Lens' InputAttachment (Core.Maybe Core.Text)
inputAttachment_inputAttachmentName = Lens.lens (\InputAttachment' {inputAttachmentName} -> inputAttachmentName) (\s@InputAttachment' {} a -> s {inputAttachmentName = a} :: InputAttachment)

-- | User-specified settings for defining what the conditions are for
-- declaring the input unhealthy and failing over to a different input.
inputAttachment_automaticInputFailoverSettings :: Lens.Lens' InputAttachment (Core.Maybe AutomaticInputFailoverSettings)
inputAttachment_automaticInputFailoverSettings = Lens.lens (\InputAttachment' {automaticInputFailoverSettings} -> automaticInputFailoverSettings) (\s@InputAttachment' {} a -> s {automaticInputFailoverSettings = a} :: InputAttachment)

instance Core.FromJSON InputAttachment where
  parseJSON =
    Core.withObject
      "InputAttachment"
      ( \x ->
          InputAttachment'
            Core.<$> (x Core..:? "inputSettings")
            Core.<*> (x Core..:? "inputId")
            Core.<*> (x Core..:? "inputAttachmentName")
            Core.<*> (x Core..:? "automaticInputFailoverSettings")
      )

instance Core.Hashable InputAttachment

instance Core.NFData InputAttachment

instance Core.ToJSON InputAttachment where
  toJSON InputAttachment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inputSettings" Core..=) Core.<$> inputSettings,
            ("inputId" Core..=) Core.<$> inputId,
            ("inputAttachmentName" Core..=)
              Core.<$> inputAttachmentName,
            ("automaticInputFailoverSettings" Core..=)
              Core.<$> automaticInputFailoverSettings
          ]
      )
