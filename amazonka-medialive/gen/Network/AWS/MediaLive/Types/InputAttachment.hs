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
-- Module      : Network.AWS.MediaLive.Types.InputAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputAttachment where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.InputSettings
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'newInputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { -- | Settings of an input (caption selector, etc.)
    inputSettings :: Prelude.Maybe InputSettings,
    -- | The ID of the input
    inputId :: Prelude.Maybe Prelude.Text,
    -- | User-specified name for the attachment. This is required if the user
    -- wants to use this input in an input switch action.
    inputAttachmentName :: Prelude.Maybe Prelude.Text,
    -- | User-specified settings for defining what the conditions are for
    -- declaring the input unhealthy and failing over to a different input.
    automaticInputFailoverSettings :: Prelude.Maybe AutomaticInputFailoverSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { inputSettings = Prelude.Nothing,
      inputId = Prelude.Nothing,
      inputAttachmentName = Prelude.Nothing,
      automaticInputFailoverSettings = Prelude.Nothing
    }

-- | Settings of an input (caption selector, etc.)
inputAttachment_inputSettings :: Lens.Lens' InputAttachment (Prelude.Maybe InputSettings)
inputAttachment_inputSettings = Lens.lens (\InputAttachment' {inputSettings} -> inputSettings) (\s@InputAttachment' {} a -> s {inputSettings = a} :: InputAttachment)

-- | The ID of the input
inputAttachment_inputId :: Lens.Lens' InputAttachment (Prelude.Maybe Prelude.Text)
inputAttachment_inputId = Lens.lens (\InputAttachment' {inputId} -> inputId) (\s@InputAttachment' {} a -> s {inputId = a} :: InputAttachment)

-- | User-specified name for the attachment. This is required if the user
-- wants to use this input in an input switch action.
inputAttachment_inputAttachmentName :: Lens.Lens' InputAttachment (Prelude.Maybe Prelude.Text)
inputAttachment_inputAttachmentName = Lens.lens (\InputAttachment' {inputAttachmentName} -> inputAttachmentName) (\s@InputAttachment' {} a -> s {inputAttachmentName = a} :: InputAttachment)

-- | User-specified settings for defining what the conditions are for
-- declaring the input unhealthy and failing over to a different input.
inputAttachment_automaticInputFailoverSettings :: Lens.Lens' InputAttachment (Prelude.Maybe AutomaticInputFailoverSettings)
inputAttachment_automaticInputFailoverSettings = Lens.lens (\InputAttachment' {automaticInputFailoverSettings} -> automaticInputFailoverSettings) (\s@InputAttachment' {} a -> s {automaticInputFailoverSettings = a} :: InputAttachment)

instance Prelude.FromJSON InputAttachment where
  parseJSON =
    Prelude.withObject
      "InputAttachment"
      ( \x ->
          InputAttachment'
            Prelude.<$> (x Prelude..:? "inputSettings")
            Prelude.<*> (x Prelude..:? "inputId")
            Prelude.<*> (x Prelude..:? "inputAttachmentName")
            Prelude.<*> (x Prelude..:? "automaticInputFailoverSettings")
      )

instance Prelude.Hashable InputAttachment

instance Prelude.NFData InputAttachment

instance Prelude.ToJSON InputAttachment where
  toJSON InputAttachment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputSettings" Prelude..=)
              Prelude.<$> inputSettings,
            ("inputId" Prelude..=) Prelude.<$> inputId,
            ("inputAttachmentName" Prelude..=)
              Prelude.<$> inputAttachmentName,
            ("automaticInputFailoverSettings" Prelude..=)
              Prelude.<$> automaticInputFailoverSettings
          ]
      )
