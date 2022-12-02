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
-- Module      : Amazonka.PinpointEmail.Types.RawMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.RawMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The raw email message.
--
-- /See:/ 'newRawMessage' smart constructor.
data RawMessage = RawMessage'
  { -- | The raw email message. The message has to meet the following criteria:
    --
    -- -   The message has to contain a header and a body, separated by one
    --     blank line.
    --
    -- -   All of the required header fields must be present in the message.
    --
    -- -   Each part of a multipart MIME message must be formatted properly.
    --
    -- -   Attachments must be in a file format that Amazon Pinpoint supports.
    --
    -- -   The entire message must be Base64 encoded.
    --
    -- -   If any of the MIME parts in your message contain content that is
    --     outside of the 7-bit ASCII character range, you should encode that
    --     content to ensure that recipients\' email clients render the message
    --     properly.
    --
    -- -   The length of any single line of text in the message can\'t exceed
    --     1,000 characters. This restriction is defined in
    --     <https://tools.ietf.org/html/rfc5321 RFC 5321>.
    data' :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RawMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'rawMessage_data' - The raw email message. The message has to meet the following criteria:
--
-- -   The message has to contain a header and a body, separated by one
--     blank line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   Attachments must be in a file format that Amazon Pinpoint supports.
--
-- -   The entire message must be Base64 encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, you should encode that
--     content to ensure that recipients\' email clients render the message
--     properly.
--
-- -   The length of any single line of text in the message can\'t exceed
--     1,000 characters. This restriction is defined in
--     <https://tools.ietf.org/html/rfc5321 RFC 5321>.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newRawMessage ::
  -- | 'data''
  Prelude.ByteString ->
  RawMessage
newRawMessage pData_ =
  RawMessage' {data' = Data._Base64 Lens.# pData_}

-- | The raw email message. The message has to meet the following criteria:
--
-- -   The message has to contain a header and a body, separated by one
--     blank line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   Attachments must be in a file format that Amazon Pinpoint supports.
--
-- -   The entire message must be Base64 encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, you should encode that
--     content to ensure that recipients\' email clients render the message
--     properly.
--
-- -   The length of any single line of text in the message can\'t exceed
--     1,000 characters. This restriction is defined in
--     <https://tools.ietf.org/html/rfc5321 RFC 5321>.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
rawMessage_data :: Lens.Lens' RawMessage Prelude.ByteString
rawMessage_data = Lens.lens (\RawMessage' {data'} -> data') (\s@RawMessage' {} a -> s {data' = a} :: RawMessage) Prelude.. Data._Base64

instance Prelude.Hashable RawMessage where
  hashWithSalt _salt RawMessage' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData RawMessage where
  rnf RawMessage' {..} = Prelude.rnf data'

instance Data.ToJSON RawMessage where
  toJSON RawMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Data" Data..= data')]
      )
