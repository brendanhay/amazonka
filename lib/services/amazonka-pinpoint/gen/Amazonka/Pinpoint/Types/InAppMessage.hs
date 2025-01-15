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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.Layout
import qualified Amazonka.Prelude as Prelude

-- | Provides all fields required for building an in-app message.
--
-- /See:/ 'newInAppMessage' smart constructor.
data InAppMessage = InAppMessage'
  { -- | In-app message content.
    content :: Prelude.Maybe [InAppMessageContent],
    -- | Custom config to be sent to SDK.
    customConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The layout of the message.
    layout :: Prelude.Maybe Layout
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
-- 'content', 'inAppMessage_content' - In-app message content.
--
-- 'customConfig', 'inAppMessage_customConfig' - Custom config to be sent to SDK.
--
-- 'layout', 'inAppMessage_layout' - The layout of the message.
newInAppMessage ::
  InAppMessage
newInAppMessage =
  InAppMessage'
    { content = Prelude.Nothing,
      customConfig = Prelude.Nothing,
      layout = Prelude.Nothing
    }

-- | In-app message content.
inAppMessage_content :: Lens.Lens' InAppMessage (Prelude.Maybe [InAppMessageContent])
inAppMessage_content = Lens.lens (\InAppMessage' {content} -> content) (\s@InAppMessage' {} a -> s {content = a} :: InAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | Custom config to be sent to SDK.
inAppMessage_customConfig :: Lens.Lens' InAppMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inAppMessage_customConfig = Lens.lens (\InAppMessage' {customConfig} -> customConfig) (\s@InAppMessage' {} a -> s {customConfig = a} :: InAppMessage) Prelude.. Lens.mapping Lens.coerced

-- | The layout of the message.
inAppMessage_layout :: Lens.Lens' InAppMessage (Prelude.Maybe Layout)
inAppMessage_layout = Lens.lens (\InAppMessage' {layout} -> layout) (\s@InAppMessage' {} a -> s {layout = a} :: InAppMessage)

instance Data.FromJSON InAppMessage where
  parseJSON =
    Data.withObject
      "InAppMessage"
      ( \x ->
          InAppMessage'
            Prelude.<$> (x Data..:? "Content" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CustomConfig" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Layout")
      )

instance Prelude.Hashable InAppMessage where
  hashWithSalt _salt InAppMessage' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` customConfig
      `Prelude.hashWithSalt` layout

instance Prelude.NFData InAppMessage where
  rnf InAppMessage' {..} =
    Prelude.rnf content `Prelude.seq`
      Prelude.rnf customConfig `Prelude.seq`
        Prelude.rnf layout
