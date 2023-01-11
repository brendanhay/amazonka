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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignInAppMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.Layout
import qualified Amazonka.Prelude as Prelude

-- | In-app message configuration.
--
-- /See:/ 'newCampaignInAppMessage' smart constructor.
data CampaignInAppMessage = CampaignInAppMessage'
  { -- | The message body of the notification, the email body or the text
    -- message.
    body :: Prelude.Maybe Prelude.Text,
    -- | In-app message content.
    content :: Prelude.Maybe [InAppMessageContent],
    -- | Custom config to be sent to client.
    customConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | In-app message layout.
    layout :: Prelude.Maybe Layout
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
-- 'body', 'campaignInAppMessage_body' - The message body of the notification, the email body or the text
-- message.
--
-- 'content', 'campaignInAppMessage_content' - In-app message content.
--
-- 'customConfig', 'campaignInAppMessage_customConfig' - Custom config to be sent to client.
--
-- 'layout', 'campaignInAppMessage_layout' - In-app message layout.
newCampaignInAppMessage ::
  CampaignInAppMessage
newCampaignInAppMessage =
  CampaignInAppMessage'
    { body = Prelude.Nothing,
      content = Prelude.Nothing,
      customConfig = Prelude.Nothing,
      layout = Prelude.Nothing
    }

-- | The message body of the notification, the email body or the text
-- message.
campaignInAppMessage_body :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe Prelude.Text)
campaignInAppMessage_body = Lens.lens (\CampaignInAppMessage' {body} -> body) (\s@CampaignInAppMessage' {} a -> s {body = a} :: CampaignInAppMessage)

-- | In-app message content.
campaignInAppMessage_content :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe [InAppMessageContent])
campaignInAppMessage_content = Lens.lens (\CampaignInAppMessage' {content} -> content) (\s@CampaignInAppMessage' {} a -> s {content = a} :: CampaignInAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | Custom config to be sent to client.
campaignInAppMessage_customConfig :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
campaignInAppMessage_customConfig = Lens.lens (\CampaignInAppMessage' {customConfig} -> customConfig) (\s@CampaignInAppMessage' {} a -> s {customConfig = a} :: CampaignInAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | In-app message layout.
campaignInAppMessage_layout :: Lens.Lens' CampaignInAppMessage (Prelude.Maybe Layout)
campaignInAppMessage_layout = Lens.lens (\CampaignInAppMessage' {layout} -> layout) (\s@CampaignInAppMessage' {} a -> s {layout = a} :: CampaignInAppMessage)

instance Data.FromJSON CampaignInAppMessage where
  parseJSON =
    Data.withObject
      "CampaignInAppMessage"
      ( \x ->
          CampaignInAppMessage'
            Prelude.<$> (x Data..:? "Body")
            Prelude.<*> (x Data..:? "Content" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CustomConfig" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Layout")
      )

instance Prelude.Hashable CampaignInAppMessage where
  hashWithSalt _salt CampaignInAppMessage' {..} =
    _salt `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` customConfig
      `Prelude.hashWithSalt` layout

instance Prelude.NFData CampaignInAppMessage where
  rnf CampaignInAppMessage' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf customConfig
      `Prelude.seq` Prelude.rnf layout

instance Data.ToJSON CampaignInAppMessage where
  toJSON CampaignInAppMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("Content" Data..=) Prelude.<$> content,
            ("CustomConfig" Data..=) Prelude.<$> customConfig,
            ("Layout" Data..=) Prelude.<$> layout
          ]
      )
