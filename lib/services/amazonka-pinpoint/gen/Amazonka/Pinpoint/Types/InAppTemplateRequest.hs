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
-- Module      : Amazonka.Pinpoint.Types.InAppTemplateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppTemplateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.Layout
import qualified Amazonka.Prelude as Prelude

-- | InApp Template Request.
--
-- /See:/ 'newInAppTemplateRequest' smart constructor.
data InAppTemplateRequest = InAppTemplateRequest'
  { -- | Custom config to be sent to client.
    customConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The layout of the message.
    layout :: Prelude.Maybe Layout,
    -- | The content of the message, can include up to 5 modals. Each modal must
    -- contain a message, a header, and background color. ImageUrl and buttons
    -- are optional.
    content :: Prelude.Maybe [InAppMessageContent],
    -- | The description of the template.
    templateDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customConfig', 'inAppTemplateRequest_customConfig' - Custom config to be sent to client.
--
-- 'tags', 'inAppTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'layout', 'inAppTemplateRequest_layout' - The layout of the message.
--
-- 'content', 'inAppTemplateRequest_content' - The content of the message, can include up to 5 modals. Each modal must
-- contain a message, a header, and background color. ImageUrl and buttons
-- are optional.
--
-- 'templateDescription', 'inAppTemplateRequest_templateDescription' - The description of the template.
newInAppTemplateRequest ::
  InAppTemplateRequest
newInAppTemplateRequest =
  InAppTemplateRequest'
    { customConfig =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      layout = Prelude.Nothing,
      content = Prelude.Nothing,
      templateDescription = Prelude.Nothing
    }

-- | Custom config to be sent to client.
inAppTemplateRequest_customConfig :: Lens.Lens' InAppTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inAppTemplateRequest_customConfig = Lens.lens (\InAppTemplateRequest' {customConfig} -> customConfig) (\s@InAppTemplateRequest' {} a -> s {customConfig = a} :: InAppTemplateRequest) Prelude.. Lens.mapping Lens.coerced

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
inAppTemplateRequest_tags :: Lens.Lens' InAppTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inAppTemplateRequest_tags = Lens.lens (\InAppTemplateRequest' {tags} -> tags) (\s@InAppTemplateRequest' {} a -> s {tags = a} :: InAppTemplateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The layout of the message.
inAppTemplateRequest_layout :: Lens.Lens' InAppTemplateRequest (Prelude.Maybe Layout)
inAppTemplateRequest_layout = Lens.lens (\InAppTemplateRequest' {layout} -> layout) (\s@InAppTemplateRequest' {} a -> s {layout = a} :: InAppTemplateRequest)

-- | The content of the message, can include up to 5 modals. Each modal must
-- contain a message, a header, and background color. ImageUrl and buttons
-- are optional.
inAppTemplateRequest_content :: Lens.Lens' InAppTemplateRequest (Prelude.Maybe [InAppMessageContent])
inAppTemplateRequest_content = Lens.lens (\InAppTemplateRequest' {content} -> content) (\s@InAppTemplateRequest' {} a -> s {content = a} :: InAppTemplateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The description of the template.
inAppTemplateRequest_templateDescription :: Lens.Lens' InAppTemplateRequest (Prelude.Maybe Prelude.Text)
inAppTemplateRequest_templateDescription = Lens.lens (\InAppTemplateRequest' {templateDescription} -> templateDescription) (\s@InAppTemplateRequest' {} a -> s {templateDescription = a} :: InAppTemplateRequest)

instance Prelude.Hashable InAppTemplateRequest where
  hashWithSalt _salt InAppTemplateRequest' {..} =
    _salt `Prelude.hashWithSalt` customConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` layout
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` templateDescription

instance Prelude.NFData InAppTemplateRequest where
  rnf InAppTemplateRequest' {..} =
    Prelude.rnf customConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf layout
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf templateDescription

instance Core.ToJSON InAppTemplateRequest where
  toJSON InAppTemplateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomConfig" Core..=) Prelude.<$> customConfig,
            ("tags" Core..=) Prelude.<$> tags,
            ("Layout" Core..=) Prelude.<$> layout,
            ("Content" Core..=) Prelude.<$> content,
            ("TemplateDescription" Core..=)
              Prelude.<$> templateDescription
          ]
      )
