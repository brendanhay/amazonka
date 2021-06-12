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
-- Module      : Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateTemplateMessageBody where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a request to create a message template.
--
-- /See:/ 'newCreateTemplateMessageBody' smart constructor.
data CreateTemplateMessageBody = CreateTemplateMessageBody'
  { -- | The message that\'s returned from the API for the request to create the
    -- message template.
    message :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the message template that was created.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier for the request to create the message template.
    requestID :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTemplateMessageBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'createTemplateMessageBody_message' - The message that\'s returned from the API for the request to create the
-- message template.
--
-- 'arn', 'createTemplateMessageBody_arn' - The Amazon Resource Name (ARN) of the message template that was created.
--
-- 'requestID', 'createTemplateMessageBody_requestID' - The unique identifier for the request to create the message template.
newCreateTemplateMessageBody ::
  CreateTemplateMessageBody
newCreateTemplateMessageBody =
  CreateTemplateMessageBody'
    { message = Core.Nothing,
      arn = Core.Nothing,
      requestID = Core.Nothing
    }

-- | The message that\'s returned from the API for the request to create the
-- message template.
createTemplateMessageBody_message :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
createTemplateMessageBody_message = Lens.lens (\CreateTemplateMessageBody' {message} -> message) (\s@CreateTemplateMessageBody' {} a -> s {message = a} :: CreateTemplateMessageBody)

-- | The Amazon Resource Name (ARN) of the message template that was created.
createTemplateMessageBody_arn :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
createTemplateMessageBody_arn = Lens.lens (\CreateTemplateMessageBody' {arn} -> arn) (\s@CreateTemplateMessageBody' {} a -> s {arn = a} :: CreateTemplateMessageBody)

-- | The unique identifier for the request to create the message template.
createTemplateMessageBody_requestID :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
createTemplateMessageBody_requestID = Lens.lens (\CreateTemplateMessageBody' {requestID} -> requestID) (\s@CreateTemplateMessageBody' {} a -> s {requestID = a} :: CreateTemplateMessageBody)

instance Core.FromJSON CreateTemplateMessageBody where
  parseJSON =
    Core.withObject
      "CreateTemplateMessageBody"
      ( \x ->
          CreateTemplateMessageBody'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "RequestID")
      )

instance Core.Hashable CreateTemplateMessageBody

instance Core.NFData CreateTemplateMessageBody
