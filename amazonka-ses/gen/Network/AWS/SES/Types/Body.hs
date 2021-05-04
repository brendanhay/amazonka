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
-- Module      : Network.AWS.SES.Types.Body
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Body where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.Content

-- | Represents the body of the message. You can specify text, HTML, or both.
-- If you use both, then the message should display correctly in the widest
-- variety of email clients.
--
-- /See:/ 'newBody' smart constructor.
data Body = Body'
  { -- | The content of the message, in HTML format. Use this for email clients
    -- that can process HTML. You can include clickable links, formatted text,
    -- and much more in an HTML message.
    html :: Prelude.Maybe Content,
    -- | The content of the message, in text format. Use this for text-based
    -- email clients, or clients on high-latency networks (such as mobile
    -- devices).
    text :: Prelude.Maybe Content
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Body' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'html', 'body_html' - The content of the message, in HTML format. Use this for email clients
-- that can process HTML. You can include clickable links, formatted text,
-- and much more in an HTML message.
--
-- 'text', 'body_text' - The content of the message, in text format. Use this for text-based
-- email clients, or clients on high-latency networks (such as mobile
-- devices).
newBody ::
  Body
newBody =
  Body'
    { html = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The content of the message, in HTML format. Use this for email clients
-- that can process HTML. You can include clickable links, formatted text,
-- and much more in an HTML message.
body_html :: Lens.Lens' Body (Prelude.Maybe Content)
body_html = Lens.lens (\Body' {html} -> html) (\s@Body' {} a -> s {html = a} :: Body)

-- | The content of the message, in text format. Use this for text-based
-- email clients, or clients on high-latency networks (such as mobile
-- devices).
body_text :: Lens.Lens' Body (Prelude.Maybe Content)
body_text = Lens.lens (\Body' {text} -> text) (\s@Body' {} a -> s {text = a} :: Body)

instance Prelude.Hashable Body

instance Prelude.NFData Body

instance Prelude.ToQuery Body where
  toQuery Body' {..} =
    Prelude.mconcat
      ["Html" Prelude.=: html, "Text" Prelude.=: text]
