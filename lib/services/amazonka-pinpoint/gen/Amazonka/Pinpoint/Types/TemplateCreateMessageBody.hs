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
-- Module      : Amazonka.Pinpoint.Types.TemplateCreateMessageBody
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateCreateMessageBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a request to create a message template.
--
-- /See:/ 'newTemplateCreateMessageBody' smart constructor.
data TemplateCreateMessageBody = TemplateCreateMessageBody'
  { -- | The message that\'s returned from the API for the request to create the
    -- message template.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the request to create the message template.
    requestID :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the message template that was created.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateCreateMessageBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'templateCreateMessageBody_message' - The message that\'s returned from the API for the request to create the
-- message template.
--
-- 'requestID', 'templateCreateMessageBody_requestID' - The unique identifier for the request to create the message template.
--
-- 'arn', 'templateCreateMessageBody_arn' - The Amazon Resource Name (ARN) of the message template that was created.
newTemplateCreateMessageBody ::
  TemplateCreateMessageBody
newTemplateCreateMessageBody =
  TemplateCreateMessageBody'
    { message =
        Prelude.Nothing,
      requestID = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The message that\'s returned from the API for the request to create the
-- message template.
templateCreateMessageBody_message :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_message = Lens.lens (\TemplateCreateMessageBody' {message} -> message) (\s@TemplateCreateMessageBody' {} a -> s {message = a} :: TemplateCreateMessageBody)

-- | The unique identifier for the request to create the message template.
templateCreateMessageBody_requestID :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_requestID = Lens.lens (\TemplateCreateMessageBody' {requestID} -> requestID) (\s@TemplateCreateMessageBody' {} a -> s {requestID = a} :: TemplateCreateMessageBody)

-- | The Amazon Resource Name (ARN) of the message template that was created.
templateCreateMessageBody_arn :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_arn = Lens.lens (\TemplateCreateMessageBody' {arn} -> arn) (\s@TemplateCreateMessageBody' {} a -> s {arn = a} :: TemplateCreateMessageBody)

instance Core.FromJSON TemplateCreateMessageBody where
  parseJSON =
    Core.withObject
      "TemplateCreateMessageBody"
      ( \x ->
          TemplateCreateMessageBody'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "RequestID")
            Prelude.<*> (x Core..:? "Arn")
      )

instance Prelude.Hashable TemplateCreateMessageBody where
  hashWithSalt _salt TemplateCreateMessageBody' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` requestID
      `Prelude.hashWithSalt` arn

instance Prelude.NFData TemplateCreateMessageBody where
  rnf TemplateCreateMessageBody' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf requestID
      `Prelude.seq` Prelude.rnf arn
