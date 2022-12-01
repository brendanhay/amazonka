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
-- Module      : Amazonka.WorkMailMessageFlow.Types.RawMessageContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMailMessageFlow.Types.RawMessageContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMailMessageFlow.Types.S3Reference

-- | Provides the MIME content of the updated email message as an S3 object.
-- All MIME content must meet the following criteria:
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   Attachments must be of a content type that Amazon SES supports. For
--     more information, see
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mime-types-appendix.html Unsupported Attachment Types>.
--
-- -   If any of the MIME parts in a message contain content that is
--     outside of the 7-bit ASCII character range, we recommend encoding
--     that content.
--
-- -   Per
--     <https://tools.ietf.org/html/rfc5321#section-4.5.3.1.6 RFC 5321>,
--     the maximum length of each line of text, including the \<CRLF>, must
--     not exceed 1,000 characters.
--
-- -   The message must contain all the required header fields. Check the
--     returned error message for more information.
--
-- -   The value of immutable headers must remain unchanged. Check the
--     returned error message for more information.
--
-- -   Certain unique headers can only appear once. Check the returned
--     error message for more information.
--
-- /See:/ 'newRawMessageContent' smart constructor.
data RawMessageContent = RawMessageContent'
  { -- | The S3 reference of an email message.
    s3Reference :: S3Reference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RawMessageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Reference', 'rawMessageContent_s3Reference' - The S3 reference of an email message.
newRawMessageContent ::
  -- | 's3Reference'
  S3Reference ->
  RawMessageContent
newRawMessageContent pS3Reference_ =
  RawMessageContent' {s3Reference = pS3Reference_}

-- | The S3 reference of an email message.
rawMessageContent_s3Reference :: Lens.Lens' RawMessageContent S3Reference
rawMessageContent_s3Reference = Lens.lens (\RawMessageContent' {s3Reference} -> s3Reference) (\s@RawMessageContent' {} a -> s {s3Reference = a} :: RawMessageContent)

instance Prelude.Hashable RawMessageContent where
  hashWithSalt _salt RawMessageContent' {..} =
    _salt `Prelude.hashWithSalt` s3Reference

instance Prelude.NFData RawMessageContent where
  rnf RawMessageContent' {..} = Prelude.rnf s3Reference

instance Core.ToJSON RawMessageContent where
  toJSON RawMessageContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3Reference" Core..= s3Reference)]
      )
