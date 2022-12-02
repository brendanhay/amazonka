{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.GetCustomDataIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the criteria and other settings for a custom data identifier.
module Amazonka.MacieV2.GetCustomDataIdentifier
  ( -- * Creating a Request
    GetCustomDataIdentifier (..),
    newGetCustomDataIdentifier,

    -- * Request Lenses
    getCustomDataIdentifier_id,

    -- * Destructuring the Response
    GetCustomDataIdentifierResponse (..),
    newGetCustomDataIdentifierResponse,

    -- * Response Lenses
    getCustomDataIdentifierResponse_tags,
    getCustomDataIdentifierResponse_name,
    getCustomDataIdentifierResponse_deleted,
    getCustomDataIdentifierResponse_regex,
    getCustomDataIdentifierResponse_arn,
    getCustomDataIdentifierResponse_ignoreWords,
    getCustomDataIdentifierResponse_keywords,
    getCustomDataIdentifierResponse_description,
    getCustomDataIdentifierResponse_id,
    getCustomDataIdentifierResponse_severityLevels,
    getCustomDataIdentifierResponse_maximumMatchDistance,
    getCustomDataIdentifierResponse_createdAt,
    getCustomDataIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCustomDataIdentifier' smart constructor.
data GetCustomDataIdentifier = GetCustomDataIdentifier'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomDataIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCustomDataIdentifier_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newGetCustomDataIdentifier ::
  -- | 'id'
  Prelude.Text ->
  GetCustomDataIdentifier
newGetCustomDataIdentifier pId_ =
  GetCustomDataIdentifier' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
getCustomDataIdentifier_id :: Lens.Lens' GetCustomDataIdentifier Prelude.Text
getCustomDataIdentifier_id = Lens.lens (\GetCustomDataIdentifier' {id} -> id) (\s@GetCustomDataIdentifier' {} a -> s {id = a} :: GetCustomDataIdentifier)

instance Core.AWSRequest GetCustomDataIdentifier where
  type
    AWSResponse GetCustomDataIdentifier =
      GetCustomDataIdentifierResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCustomDataIdentifierResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "deleted")
            Prelude.<*> (x Data..?> "regex")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "ignoreWords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "keywords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "severityLevels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "maximumMatchDistance")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCustomDataIdentifier where
  hashWithSalt _salt GetCustomDataIdentifier' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetCustomDataIdentifier where
  rnf GetCustomDataIdentifier' {..} = Prelude.rnf id

instance Data.ToHeaders GetCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCustomDataIdentifier where
  toPath GetCustomDataIdentifier' {..} =
    Prelude.mconcat
      ["/custom-data-identifiers/", Data.toBS id]

instance Data.ToQuery GetCustomDataIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCustomDataIdentifierResponse' smart constructor.
data GetCustomDataIdentifierResponse = GetCustomDataIdentifierResponse'
  { -- | A map of key-value pairs that identifies the tags (keys and values) that
    -- are associated with the custom data identifier.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The custom name of the custom data identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the custom data identifier was deleted. If you delete
    -- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
    -- Instead, it soft deletes the identifier.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The regular expression (/regex/) that defines the pattern to match.
    regex :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom data identifier.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An array that lists specific character sequences (/ignore words/) to
    -- exclude from the results. If the text matched by the regular expression
    -- contains any string in this array, Amazon Macie ignores it. Ignore words
    -- are case sensitive.
    ignoreWords :: Prelude.Maybe [Prelude.Text],
    -- | An array that lists specific character sequences (/keywords/), one of
    -- which must precede and be within proximity (maximumMatchDistance) of the
    -- regular expression to match. Keywords aren\'t case sensitive.
    keywords :: Prelude.Maybe [Prelude.Text],
    -- | The custom description of the custom data identifier.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the custom data identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies the severity that\'s assigned to findings that the custom data
    -- identifier produces, based on the number of occurrences of text that
    -- matches the custom data identifier\'s detection criteria. By default,
    -- Amazon Macie creates findings for S3 objects that contain at least one
    -- occurrence of text that matches the detection criteria, and Macie
    -- assigns the MEDIUM severity to those findings.
    severityLevels :: Prelude.Maybe [SeverityLevel],
    -- | The maximum number of characters that can exist between the end of at
    -- least one complete character sequence specified by the keywords array
    -- and the end of the text that matches the regex pattern. If a complete
    -- keyword precedes all the text that matches the pattern and the keyword
    -- is within the specified distance, Amazon Macie includes the result.
    -- Otherwise, Macie excludes the result.
    maximumMatchDistance :: Prelude.Maybe Prelude.Int,
    -- | The date and time, in UTC and extended ISO 8601 format, when the custom
    -- data identifier was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomDataIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getCustomDataIdentifierResponse_tags' - A map of key-value pairs that identifies the tags (keys and values) that
-- are associated with the custom data identifier.
--
-- 'name', 'getCustomDataIdentifierResponse_name' - The custom name of the custom data identifier.
--
-- 'deleted', 'getCustomDataIdentifierResponse_deleted' - Specifies whether the custom data identifier was deleted. If you delete
-- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
-- Instead, it soft deletes the identifier.
--
-- 'regex', 'getCustomDataIdentifierResponse_regex' - The regular expression (/regex/) that defines the pattern to match.
--
-- 'arn', 'getCustomDataIdentifierResponse_arn' - The Amazon Resource Name (ARN) of the custom data identifier.
--
-- 'ignoreWords', 'getCustomDataIdentifierResponse_ignoreWords' - An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. Ignore words
-- are case sensitive.
--
-- 'keywords', 'getCustomDataIdentifierResponse_keywords' - An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. Keywords aren\'t case sensitive.
--
-- 'description', 'getCustomDataIdentifierResponse_description' - The custom description of the custom data identifier.
--
-- 'id', 'getCustomDataIdentifierResponse_id' - The unique identifier for the custom data identifier.
--
-- 'severityLevels', 'getCustomDataIdentifierResponse_severityLevels' - Specifies the severity that\'s assigned to findings that the custom data
-- identifier produces, based on the number of occurrences of text that
-- matches the custom data identifier\'s detection criteria. By default,
-- Amazon Macie creates findings for S3 objects that contain at least one
-- occurrence of text that matches the detection criteria, and Macie
-- assigns the MEDIUM severity to those findings.
--
-- 'maximumMatchDistance', 'getCustomDataIdentifierResponse_maximumMatchDistance' - The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result.
-- Otherwise, Macie excludes the result.
--
-- 'createdAt', 'getCustomDataIdentifierResponse_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
--
-- 'httpStatus', 'getCustomDataIdentifierResponse_httpStatus' - The response's http status code.
newGetCustomDataIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCustomDataIdentifierResponse
newGetCustomDataIdentifierResponse pHttpStatus_ =
  GetCustomDataIdentifierResponse'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      deleted = Prelude.Nothing,
      regex = Prelude.Nothing,
      arn = Prelude.Nothing,
      ignoreWords = Prelude.Nothing,
      keywords = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      severityLevels = Prelude.Nothing,
      maximumMatchDistance = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of key-value pairs that identifies the tags (keys and values) that
-- are associated with the custom data identifier.
getCustomDataIdentifierResponse_tags :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCustomDataIdentifierResponse_tags = Lens.lens (\GetCustomDataIdentifierResponse' {tags} -> tags) (\s@GetCustomDataIdentifierResponse' {} a -> s {tags = a} :: GetCustomDataIdentifierResponse) Prelude.. Lens.mapping Lens.coerced

-- | The custom name of the custom data identifier.
getCustomDataIdentifierResponse_name :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
getCustomDataIdentifierResponse_name = Lens.lens (\GetCustomDataIdentifierResponse' {name} -> name) (\s@GetCustomDataIdentifierResponse' {} a -> s {name = a} :: GetCustomDataIdentifierResponse)

-- | Specifies whether the custom data identifier was deleted. If you delete
-- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
-- Instead, it soft deletes the identifier.
getCustomDataIdentifierResponse_deleted :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Bool)
getCustomDataIdentifierResponse_deleted = Lens.lens (\GetCustomDataIdentifierResponse' {deleted} -> deleted) (\s@GetCustomDataIdentifierResponse' {} a -> s {deleted = a} :: GetCustomDataIdentifierResponse)

-- | The regular expression (/regex/) that defines the pattern to match.
getCustomDataIdentifierResponse_regex :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
getCustomDataIdentifierResponse_regex = Lens.lens (\GetCustomDataIdentifierResponse' {regex} -> regex) (\s@GetCustomDataIdentifierResponse' {} a -> s {regex = a} :: GetCustomDataIdentifierResponse)

-- | The Amazon Resource Name (ARN) of the custom data identifier.
getCustomDataIdentifierResponse_arn :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
getCustomDataIdentifierResponse_arn = Lens.lens (\GetCustomDataIdentifierResponse' {arn} -> arn) (\s@GetCustomDataIdentifierResponse' {} a -> s {arn = a} :: GetCustomDataIdentifierResponse)

-- | An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. Ignore words
-- are case sensitive.
getCustomDataIdentifierResponse_ignoreWords :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe [Prelude.Text])
getCustomDataIdentifierResponse_ignoreWords = Lens.lens (\GetCustomDataIdentifierResponse' {ignoreWords} -> ignoreWords) (\s@GetCustomDataIdentifierResponse' {} a -> s {ignoreWords = a} :: GetCustomDataIdentifierResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. Keywords aren\'t case sensitive.
getCustomDataIdentifierResponse_keywords :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe [Prelude.Text])
getCustomDataIdentifierResponse_keywords = Lens.lens (\GetCustomDataIdentifierResponse' {keywords} -> keywords) (\s@GetCustomDataIdentifierResponse' {} a -> s {keywords = a} :: GetCustomDataIdentifierResponse) Prelude.. Lens.mapping Lens.coerced

-- | The custom description of the custom data identifier.
getCustomDataIdentifierResponse_description :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
getCustomDataIdentifierResponse_description = Lens.lens (\GetCustomDataIdentifierResponse' {description} -> description) (\s@GetCustomDataIdentifierResponse' {} a -> s {description = a} :: GetCustomDataIdentifierResponse)

-- | The unique identifier for the custom data identifier.
getCustomDataIdentifierResponse_id :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
getCustomDataIdentifierResponse_id = Lens.lens (\GetCustomDataIdentifierResponse' {id} -> id) (\s@GetCustomDataIdentifierResponse' {} a -> s {id = a} :: GetCustomDataIdentifierResponse)

-- | Specifies the severity that\'s assigned to findings that the custom data
-- identifier produces, based on the number of occurrences of text that
-- matches the custom data identifier\'s detection criteria. By default,
-- Amazon Macie creates findings for S3 objects that contain at least one
-- occurrence of text that matches the detection criteria, and Macie
-- assigns the MEDIUM severity to those findings.
getCustomDataIdentifierResponse_severityLevels :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe [SeverityLevel])
getCustomDataIdentifierResponse_severityLevels = Lens.lens (\GetCustomDataIdentifierResponse' {severityLevels} -> severityLevels) (\s@GetCustomDataIdentifierResponse' {} a -> s {severityLevels = a} :: GetCustomDataIdentifierResponse) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result.
-- Otherwise, Macie excludes the result.
getCustomDataIdentifierResponse_maximumMatchDistance :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.Int)
getCustomDataIdentifierResponse_maximumMatchDistance = Lens.lens (\GetCustomDataIdentifierResponse' {maximumMatchDistance} -> maximumMatchDistance) (\s@GetCustomDataIdentifierResponse' {} a -> s {maximumMatchDistance = a} :: GetCustomDataIdentifierResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
getCustomDataIdentifierResponse_createdAt :: Lens.Lens' GetCustomDataIdentifierResponse (Prelude.Maybe Prelude.UTCTime)
getCustomDataIdentifierResponse_createdAt = Lens.lens (\GetCustomDataIdentifierResponse' {createdAt} -> createdAt) (\s@GetCustomDataIdentifierResponse' {} a -> s {createdAt = a} :: GetCustomDataIdentifierResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getCustomDataIdentifierResponse_httpStatus :: Lens.Lens' GetCustomDataIdentifierResponse Prelude.Int
getCustomDataIdentifierResponse_httpStatus = Lens.lens (\GetCustomDataIdentifierResponse' {httpStatus} -> httpStatus) (\s@GetCustomDataIdentifierResponse' {} a -> s {httpStatus = a} :: GetCustomDataIdentifierResponse)

instance
  Prelude.NFData
    GetCustomDataIdentifierResponse
  where
  rnf GetCustomDataIdentifierResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf deleted
      `Prelude.seq` Prelude.rnf regex
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf ignoreWords
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf severityLevels
      `Prelude.seq` Prelude.rnf maximumMatchDistance
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
