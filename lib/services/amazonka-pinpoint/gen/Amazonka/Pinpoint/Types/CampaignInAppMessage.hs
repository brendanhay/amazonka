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
-- Module      : Amazonka.Pinpoint.Types.CampaignInAppMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignInAppMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.Layout
import qualified Amazonka.Prelude as Prelude

-- | In-app message configuration.
--
-- /See:/ 'newCampaignInAppMessage' smart constructor.
data CampaignInAppMessage = CampaignInAppMessage'
  { -- | Custom config to be sent to client.
    customConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The message body of the notification, the email body or the text
    -- message.
    body :: Prelude.Maybe Prelude.Text,
    -- | In-app message layout.
    layout :: Prelude.Maybe Layout,
    -- | In-app message content.
    content :: Prelude.Maybe [InAppMessageContent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignInAppMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customConfig', 'campaignInAppMessage_customConfig' - Custom config to be sent to client.
--
-- 'body', 'campaignInAppMessage_body' - The message body of the notification, the email body or the text
-- message.
--
-- 'layout', 'campaignInAppMessage_layout' - In-app message layout.
--
-- 'content', 'campaignInAppMessage_content' - In-app message content.
newCampaignInAppMessage ::
  CampaignInAppMessage
newCampaignInAppMessage =
  CampaignInAppMessage'
    { customConfig =
        Prelude.Nothing,
      body = Prelude.Nothing,
      layout = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | Custom config to be sent to client.
campaignInAppMessage_customConfig :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
campaignInAppMessage_customConfig = Lens.lens (\CampaignInAppMessage' {customConfig} -> customConfig) (\s@CampaignInAppMessage' {} a -> s {customConfig = a} :: CampaignInAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | The message body of the notification, the email body or the text
-- message.
campaignInAppMessage_body :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe Prelude.Text)
campaignInAppMessage_body = Lens.lens (\CampaignInAppMessage' {body} -> body) (\s@CampaignInAppMessage' {} a -> s {body = a} :: CampaignInAppMessage)

-- | In-app message layout.
campaignInAppMessage_layout :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe Layout)
campaignInAppMessage_layout = Lens.lens (\CampaignInAppMessage' {layout} -> layout) (\s@CampaignInAppMessage' {} a -> s {layout = a} :: CampaignInAppMessage)

-- | In-app message content.
campaignInAppMessage_content :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe [InAppMessageContent])
campaignInAppMessage_content = Lens.lens (\CampaignInAppMessage' {content} -> content) (\s@CampaignInAppMessage' {} a -> s {content = a} :: CampaignInAppMessage) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CampaignInAppMessage where
  parseJSON =
    Core.withObject
      "CampaignInAppMessage"
      ( \x ->
          CampaignInAppMessage'
            Prelude.<$> (x Core..:? "CustomConfig" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Body")
            Prelude.<*> (x Core..:? "Layout")
            Prelude.<*> (x Core..:? "Content" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CampaignInAppMessage where
  hashWithSalt _salt CampaignInAppMessage' {..} =
    _salt `Prelude.hashWithSalt` customConfig
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` layout
      `Prelude.hashWithSalt` content

instance Prelude.NFData CampaignInAppMessage where
  rnf CampaignInAppMessage' {..} =
    Prelude.rnf customConfig
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf layout
      `Prelude.seq` Prelude.rnf content

instance Core.ToJSON CampaignInAppMessage where
  toJSON CampaignInAppMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomConfig" Core..=) Prelude.<$> customConfig,
            ("Body" Core..=) Prelude.<$> body,
            ("Layout" Core..=) Prelude.<$> layout,
            ("Content" Core..=) Prelude.<$> content
          ]
      )
