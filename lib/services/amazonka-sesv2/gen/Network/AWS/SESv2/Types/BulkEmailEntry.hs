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
-- Module      : Network.AWS.SESv2.Types.BulkEmailEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.BulkEmailEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.Destination
import Network.AWS.SESv2.Types.MessageTag
import Network.AWS.SESv2.Types.ReplacementEmailContent

-- | /See:/ 'newBulkEmailEntry' smart constructor.
data BulkEmailEntry = BulkEmailEntry'
  { -- | The @ReplacementEmailContent@ associated with a @BulkEmailEntry@.
    replacementEmailContent :: Prelude.Maybe ReplacementEmailContent,
    -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using the @SendBulkTemplatedEmail@ operation. Tags
    -- correspond to characteristics of the email that you define, so that you
    -- can publish email sending events.
    replacementTags :: Prelude.Maybe [MessageTag],
    -- | Represents the destination of the message, consisting of To:, CC:, and
    -- BCC: fields.
    --
    -- Amazon SES does not support the SMTPUTF8 extension, as described in
    -- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
    -- local part of a destination email address (the part of the email address
    -- that precedes the \@ sign) may only contain
    -- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
    -- If the domain part of an address (the part after the \@ sign) contains
    -- non-ASCII characters, they must be encoded using Punycode, as described
    -- in <https://tools.ietf.org/html/rfc3492.html RFC3492>.
    destination :: Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkEmailEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementEmailContent', 'bulkEmailEntry_replacementEmailContent' - The @ReplacementEmailContent@ associated with a @BulkEmailEntry@.
--
-- 'replacementTags', 'bulkEmailEntry_replacementTags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendBulkTemplatedEmail@ operation. Tags
-- correspond to characteristics of the email that you define, so that you
-- can publish email sending events.
--
-- 'destination', 'bulkEmailEntry_destination' - Represents the destination of the message, consisting of To:, CC:, and
-- BCC: fields.
--
-- Amazon SES does not support the SMTPUTF8 extension, as described in
-- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
-- local part of a destination email address (the part of the email address
-- that precedes the \@ sign) may only contain
-- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
-- If the domain part of an address (the part after the \@ sign) contains
-- non-ASCII characters, they must be encoded using Punycode, as described
-- in <https://tools.ietf.org/html/rfc3492.html RFC3492>.
newBulkEmailEntry ::
  -- | 'destination'
  Destination ->
  BulkEmailEntry
newBulkEmailEntry pDestination_ =
  BulkEmailEntry'
    { replacementEmailContent =
        Prelude.Nothing,
      replacementTags = Prelude.Nothing,
      destination = pDestination_
    }

-- | The @ReplacementEmailContent@ associated with a @BulkEmailEntry@.
bulkEmailEntry_replacementEmailContent :: Lens.Lens' BulkEmailEntry (Prelude.Maybe ReplacementEmailContent)
bulkEmailEntry_replacementEmailContent = Lens.lens (\BulkEmailEntry' {replacementEmailContent} -> replacementEmailContent) (\s@BulkEmailEntry' {} a -> s {replacementEmailContent = a} :: BulkEmailEntry)

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using the @SendBulkTemplatedEmail@ operation. Tags
-- correspond to characteristics of the email that you define, so that you
-- can publish email sending events.
bulkEmailEntry_replacementTags :: Lens.Lens' BulkEmailEntry (Prelude.Maybe [MessageTag])
bulkEmailEntry_replacementTags = Lens.lens (\BulkEmailEntry' {replacementTags} -> replacementTags) (\s@BulkEmailEntry' {} a -> s {replacementTags = a} :: BulkEmailEntry) Prelude.. Lens.mapping Lens.coerced

-- | Represents the destination of the message, consisting of To:, CC:, and
-- BCC: fields.
--
-- Amazon SES does not support the SMTPUTF8 extension, as described in
-- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
-- local part of a destination email address (the part of the email address
-- that precedes the \@ sign) may only contain
-- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
-- If the domain part of an address (the part after the \@ sign) contains
-- non-ASCII characters, they must be encoded using Punycode, as described
-- in <https://tools.ietf.org/html/rfc3492.html RFC3492>.
bulkEmailEntry_destination :: Lens.Lens' BulkEmailEntry Destination
bulkEmailEntry_destination = Lens.lens (\BulkEmailEntry' {destination} -> destination) (\s@BulkEmailEntry' {} a -> s {destination = a} :: BulkEmailEntry)

instance Prelude.Hashable BulkEmailEntry

instance Prelude.NFData BulkEmailEntry

instance Core.ToJSON BulkEmailEntry where
  toJSON BulkEmailEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ReplacementEmailContent" Core..=)
              Prelude.<$> replacementEmailContent,
            ("ReplacementTags" Core..=)
              Prelude.<$> replacementTags,
            Prelude.Just ("Destination" Core..= destination)
          ]
      )
