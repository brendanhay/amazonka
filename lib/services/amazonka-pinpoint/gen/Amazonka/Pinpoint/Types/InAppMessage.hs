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
-- Module      : Amazonka.Pinpoint.Types.InAppMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.Layout
import qualified Amazonka.Prelude as Prelude

-- | Provides all fields required for building an in-app message.
--
-- /See:/ 'newInAppMessage' smart constructor.
data InAppMessage = InAppMessage'
  { -- | Custom config to be sent to SDK.
    customConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The layout of the message.
    layout :: Prelude.Maybe Layout,
    -- | In-app message content.
    content :: Prelude.Maybe [InAppMessageContent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customConfig', 'inAppMessage_customConfig' - Custom config to be sent to SDK.
--
-- 'layout', 'inAppMessage_layout' - The layout of the message.
--
-- 'content', 'inAppMessage_content' - In-app message content.
newInAppMessage ::
  InAppMessage
newInAppMessage =
  InAppMessage'
    { customConfig = Prelude.Nothing,
      layout = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | Custom config to be sent to SDK.
inAppMessage_customConfig :: Lens.Lens' InAppMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inAppMessage_customConfig = Lens.lens (\InAppMessage' {customConfig} -> customConfig) (\s@InAppMessage' {} a -> s {customConfig = a} :: InAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | The layout of the message.
inAppMessage_layout :: Lens.Lens' InAppMessage (Prelude.Maybe Layout)
inAppMessage_layout = Lens.lens (\InAppMessage' {layout} -> layout) (\s@InAppMessage' {} a -> s {layout = a} :: InAppMessage)

-- | In-app message content.
inAppMessage_content :: Lens.Lens' InAppMessage (Prelude.Maybe [InAppMessageContent])
inAppMessage_content = Lens.lens (\InAppMessage' {content} -> content) (\s@InAppMessage' {} a -> s {content = a} :: InAppMessage) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON InAppMessage where
  parseJSON =
    Core.withObject
      "InAppMessage"
      ( \x ->
          InAppMessage'
            Prelude.<$> (x Core..:? "CustomConfig" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Layout")
            Prelude.<*> (x Core..:? "Content" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable InAppMessage where
  hashWithSalt _salt InAppMessage' {..} =
    _salt `Prelude.hashWithSalt` customConfig
      `Prelude.hashWithSalt` layout
      `Prelude.hashWithSalt` content

instance Prelude.NFData InAppMessage where
  rnf InAppMessage' {..} =
    Prelude.rnf customConfig
      `Prelude.seq` Prelude.rnf layout
      `Prelude.seq` Prelude.rnf content
