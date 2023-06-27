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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateCreateMessageBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a request to create a message template.
--
-- /See:/ 'newTemplateCreateMessageBody' smart constructor.
data TemplateCreateMessageBody = TemplateCreateMessageBody'
  { -- | The Amazon Resource Name (ARN) of the message template that was created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The message that\'s returned from the API for the request to create the
    -- message template.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the request to create the message template.
    requestID :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'templateCreateMessageBody_arn' - The Amazon Resource Name (ARN) of the message template that was created.
--
-- 'message', 'templateCreateMessageBody_message' - The message that\'s returned from the API for the request to create the
-- message template.
--
-- 'requestID', 'templateCreateMessageBody_requestID' - The unique identifier for the request to create the message template.
newTemplateCreateMessageBody ::
  TemplateCreateMessageBody
newTemplateCreateMessageBody =
  TemplateCreateMessageBody'
    { arn = Prelude.Nothing,
      message = Prelude.Nothing,
      requestID = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the message template that was created.
templateCreateMessageBody_arn :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_arn = Lens.lens (\TemplateCreateMessageBody' {arn} -> arn) (\s@TemplateCreateMessageBody' {} a -> s {arn = a} :: TemplateCreateMessageBody)

-- | The message that\'s returned from the API for the request to create the
-- message template.
templateCreateMessageBody_message :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_message = Lens.lens (\TemplateCreateMessageBody' {message} -> message) (\s@TemplateCreateMessageBody' {} a -> s {message = a} :: TemplateCreateMessageBody)

-- | The unique identifier for the request to create the message template.
templateCreateMessageBody_requestID :: Lens.Lens' TemplateCreateMessageBody (Prelude.Maybe Prelude.Text)
templateCreateMessageBody_requestID = Lens.lens (\TemplateCreateMessageBody' {requestID} -> requestID) (\s@TemplateCreateMessageBody' {} a -> s {requestID = a} :: TemplateCreateMessageBody)

instance Data.FromJSON TemplateCreateMessageBody where
  parseJSON =
    Data.withObject
      "TemplateCreateMessageBody"
      ( \x ->
          TemplateCreateMessageBody'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "RequestID")
      )

instance Prelude.Hashable TemplateCreateMessageBody where
  hashWithSalt _salt TemplateCreateMessageBody' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` requestID

instance Prelude.NFData TemplateCreateMessageBody where
  rnf TemplateCreateMessageBody' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf requestID
