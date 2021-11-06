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
-- Module      : Amazonka.Pinpoint.Types.InAppMessageContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessageContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.InAppMessageBodyConfig
import Amazonka.Pinpoint.Types.InAppMessageButton
import Amazonka.Pinpoint.Types.InAppMessageHeaderConfig
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the message content.
--
-- /See:/ 'newInAppMessageContent' smart constructor.
data InAppMessageContent = InAppMessageContent'
  { -- | The first button inside the message.
    primaryBtn :: Prelude.Maybe InAppMessageButton,
    -- | The configuration for the message body.
    bodyConfig :: Prelude.Maybe InAppMessageBodyConfig,
    -- | The background color for the message.
    backgroundColor :: Prelude.Maybe Prelude.Text,
    -- | The image url for the background of message.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The second button inside message.
    secondaryBtn :: Prelude.Maybe InAppMessageButton,
    -- | The configuration for the message header.
    headerConfig :: Prelude.Maybe InAppMessageHeaderConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryBtn', 'inAppMessageContent_primaryBtn' - The first button inside the message.
--
-- 'bodyConfig', 'inAppMessageContent_bodyConfig' - The configuration for the message body.
--
-- 'backgroundColor', 'inAppMessageContent_backgroundColor' - The background color for the message.
--
-- 'imageUrl', 'inAppMessageContent_imageUrl' - The image url for the background of message.
--
-- 'secondaryBtn', 'inAppMessageContent_secondaryBtn' - The second button inside message.
--
-- 'headerConfig', 'inAppMessageContent_headerConfig' - The configuration for the message header.
newInAppMessageContent ::
  InAppMessageContent
newInAppMessageContent =
  InAppMessageContent'
    { primaryBtn = Prelude.Nothing,
      bodyConfig = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      secondaryBtn = Prelude.Nothing,
      headerConfig = Prelude.Nothing
    }

-- | The first button inside the message.
inAppMessageContent_primaryBtn :: Lens.Lens' InAppMessageContent (Prelude.Maybe InAppMessageButton)
inAppMessageContent_primaryBtn = Lens.lens (\InAppMessageContent' {primaryBtn} -> primaryBtn) (\s@InAppMessageContent' {} a -> s {primaryBtn = a} :: InAppMessageContent)

-- | The configuration for the message body.
inAppMessageContent_bodyConfig :: Lens.Lens' InAppMessageContent (Prelude.Maybe InAppMessageBodyConfig)
inAppMessageContent_bodyConfig = Lens.lens (\InAppMessageContent' {bodyConfig} -> bodyConfig) (\s@InAppMessageContent' {} a -> s {bodyConfig = a} :: InAppMessageContent)

-- | The background color for the message.
inAppMessageContent_backgroundColor :: Lens.Lens' InAppMessageContent (Prelude.Maybe Prelude.Text)
inAppMessageContent_backgroundColor = Lens.lens (\InAppMessageContent' {backgroundColor} -> backgroundColor) (\s@InAppMessageContent' {} a -> s {backgroundColor = a} :: InAppMessageContent)

-- | The image url for the background of message.
inAppMessageContent_imageUrl :: Lens.Lens' InAppMessageContent (Prelude.Maybe Prelude.Text)
inAppMessageContent_imageUrl = Lens.lens (\InAppMessageContent' {imageUrl} -> imageUrl) (\s@InAppMessageContent' {} a -> s {imageUrl = a} :: InAppMessageContent)

-- | The second button inside message.
inAppMessageContent_secondaryBtn :: Lens.Lens' InAppMessageContent (Prelude.Maybe InAppMessageButton)
inAppMessageContent_secondaryBtn = Lens.lens (\InAppMessageContent' {secondaryBtn} -> secondaryBtn) (\s@InAppMessageContent' {} a -> s {secondaryBtn = a} :: InAppMessageContent)

-- | The configuration for the message header.
inAppMessageContent_headerConfig :: Lens.Lens' InAppMessageContent (Prelude.Maybe InAppMessageHeaderConfig)
inAppMessageContent_headerConfig = Lens.lens (\InAppMessageContent' {headerConfig} -> headerConfig) (\s@InAppMessageContent' {} a -> s {headerConfig = a} :: InAppMessageContent)

instance Core.FromJSON InAppMessageContent where
  parseJSON =
    Core.withObject
      "InAppMessageContent"
      ( \x ->
          InAppMessageContent'
            Prelude.<$> (x Core..:? "PrimaryBtn")
            Prelude.<*> (x Core..:? "BodyConfig")
            Prelude.<*> (x Core..:? "BackgroundColor")
            Prelude.<*> (x Core..:? "ImageUrl")
            Prelude.<*> (x Core..:? "SecondaryBtn")
            Prelude.<*> (x Core..:? "HeaderConfig")
      )

instance Prelude.Hashable InAppMessageContent

instance Prelude.NFData InAppMessageContent

instance Core.ToJSON InAppMessageContent where
  toJSON InAppMessageContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PrimaryBtn" Core..=) Prelude.<$> primaryBtn,
            ("BodyConfig" Core..=) Prelude.<$> bodyConfig,
            ("BackgroundColor" Core..=)
              Prelude.<$> backgroundColor,
            ("ImageUrl" Core..=) Prelude.<$> imageUrl,
            ("SecondaryBtn" Core..=) Prelude.<$> secondaryBtn,
            ("HeaderConfig" Core..=) Prelude.<$> headerConfig
          ]
      )
