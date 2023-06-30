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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeKeywords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified keywords or all keywords on your origination
-- phone number or pool.
--
-- A keyword is a word that you can search for on a particular phone number
-- or pool. It is also a specific word or phrase that an end user can send
-- to your number to elicit a response, such as an informational message or
-- a special offer. When your number receives a message that begins with a
-- keyword, Amazon Pinpoint responds with a customizable message.
--
-- If you specify a keyword that isn\'t valid, an Error is returned.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeKeywords
  ( -- * Creating a Request
    DescribeKeywords (..),
    newDescribeKeywords,

    -- * Request Lenses
    describeKeywords_filters,
    describeKeywords_keywords,
    describeKeywords_maxResults,
    describeKeywords_nextToken,
    describeKeywords_originationIdentity,

    -- * Destructuring the Response
    DescribeKeywordsResponse (..),
    newDescribeKeywordsResponse,

    -- * Response Lenses
    describeKeywordsResponse_keywords,
    describeKeywordsResponse_nextToken,
    describeKeywordsResponse_originationIdentity,
    describeKeywordsResponse_originationIdentityArn,
    describeKeywordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKeywords' smart constructor.
data DescribeKeywords = DescribeKeywords'
  { -- | An array of keyword filters to filter the results.
    filters :: Prelude.Maybe [KeywordFilter],
    -- | An array of keywords to search for.
    keywords :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
    -- SenderId or SenderIdArn. You can use DescribePhoneNumbers to find the
    -- values for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can
    -- be used to get the values for SenderId and SenderIdArn.
    originationIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeywords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeKeywords_filters' - An array of keyword filters to filter the results.
--
-- 'keywords', 'describeKeywords_keywords' - An array of keywords to search for.
--
-- 'maxResults', 'describeKeywords_maxResults' - The maximum number of results to return per each request.
--
-- 'nextToken', 'describeKeywords_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'originationIdentity', 'describeKeywords_originationIdentity' - The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers to find the
-- values for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can
-- be used to get the values for SenderId and SenderIdArn.
newDescribeKeywords ::
  -- | 'originationIdentity'
  Prelude.Text ->
  DescribeKeywords
newDescribeKeywords pOriginationIdentity_ =
  DescribeKeywords'
    { filters = Prelude.Nothing,
      keywords = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      originationIdentity = pOriginationIdentity_
    }

-- | An array of keyword filters to filter the results.
describeKeywords_filters :: Lens.Lens' DescribeKeywords (Prelude.Maybe [KeywordFilter])
describeKeywords_filters = Lens.lens (\DescribeKeywords' {filters} -> filters) (\s@DescribeKeywords' {} a -> s {filters = a} :: DescribeKeywords) Prelude.. Lens.mapping Lens.coerced

-- | An array of keywords to search for.
describeKeywords_keywords :: Lens.Lens' DescribeKeywords (Prelude.Maybe [Prelude.Text])
describeKeywords_keywords = Lens.lens (\DescribeKeywords' {keywords} -> keywords) (\s@DescribeKeywords' {} a -> s {keywords = a} :: DescribeKeywords) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
describeKeywords_maxResults :: Lens.Lens' DescribeKeywords (Prelude.Maybe Prelude.Natural)
describeKeywords_maxResults = Lens.lens (\DescribeKeywords' {maxResults} -> maxResults) (\s@DescribeKeywords' {} a -> s {maxResults = a} :: DescribeKeywords)

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeKeywords_nextToken :: Lens.Lens' DescribeKeywords (Prelude.Maybe Prelude.Text)
describeKeywords_nextToken = Lens.lens (\DescribeKeywords' {nextToken} -> nextToken) (\s@DescribeKeywords' {} a -> s {nextToken = a} :: DescribeKeywords)

-- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers to find the
-- values for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can
-- be used to get the values for SenderId and SenderIdArn.
describeKeywords_originationIdentity :: Lens.Lens' DescribeKeywords Prelude.Text
describeKeywords_originationIdentity = Lens.lens (\DescribeKeywords' {originationIdentity} -> originationIdentity) (\s@DescribeKeywords' {} a -> s {originationIdentity = a} :: DescribeKeywords)

instance Core.AWSPager DescribeKeywords where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeKeywordsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeKeywordsResponse_keywords
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeKeywords_nextToken
          Lens..~ rs
          Lens.^? describeKeywordsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeKeywords where
  type
    AWSResponse DescribeKeywords =
      DescribeKeywordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeywordsResponse'
            Prelude.<$> (x Data..?> "Keywords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "OriginationIdentity")
            Prelude.<*> (x Data..?> "OriginationIdentityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeKeywords where
  hashWithSalt _salt DescribeKeywords' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` originationIdentity

instance Prelude.NFData DescribeKeywords where
  rnf DescribeKeywords' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf originationIdentity

instance Data.ToHeaders DescribeKeywords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeKeywords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeKeywords where
  toJSON DescribeKeywords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Keywords" Data..=) Prelude.<$> keywords,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OriginationIdentity" Data..= originationIdentity)
          ]
      )

instance Data.ToPath DescribeKeywords where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeKeywords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeKeywordsResponse' smart constructor.
data DescribeKeywordsResponse = DescribeKeywordsResponse'
  { -- | An array of KeywordInformation objects that contain the results.
    keywords :: Prelude.Maybe [KeywordInformation],
    -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberId or PoolId that is associated with the
    -- OriginationIdentity.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberArn or PoolArn that is associated with the
    -- OriginationIdentity.
    originationIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeywordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keywords', 'describeKeywordsResponse_keywords' - An array of KeywordInformation objects that contain the results.
--
-- 'nextToken', 'describeKeywordsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'originationIdentity', 'describeKeywordsResponse_originationIdentity' - The PhoneNumberId or PoolId that is associated with the
-- OriginationIdentity.
--
-- 'originationIdentityArn', 'describeKeywordsResponse_originationIdentityArn' - The PhoneNumberArn or PoolArn that is associated with the
-- OriginationIdentity.
--
-- 'httpStatus', 'describeKeywordsResponse_httpStatus' - The response's http status code.
newDescribeKeywordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeKeywordsResponse
newDescribeKeywordsResponse pHttpStatus_ =
  DescribeKeywordsResponse'
    { keywords =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      originationIdentity = Prelude.Nothing,
      originationIdentityArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of KeywordInformation objects that contain the results.
describeKeywordsResponse_keywords :: Lens.Lens' DescribeKeywordsResponse (Prelude.Maybe [KeywordInformation])
describeKeywordsResponse_keywords = Lens.lens (\DescribeKeywordsResponse' {keywords} -> keywords) (\s@DescribeKeywordsResponse' {} a -> s {keywords = a} :: DescribeKeywordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeKeywordsResponse_nextToken :: Lens.Lens' DescribeKeywordsResponse (Prelude.Maybe Prelude.Text)
describeKeywordsResponse_nextToken = Lens.lens (\DescribeKeywordsResponse' {nextToken} -> nextToken) (\s@DescribeKeywordsResponse' {} a -> s {nextToken = a} :: DescribeKeywordsResponse)

-- | The PhoneNumberId or PoolId that is associated with the
-- OriginationIdentity.
describeKeywordsResponse_originationIdentity :: Lens.Lens' DescribeKeywordsResponse (Prelude.Maybe Prelude.Text)
describeKeywordsResponse_originationIdentity = Lens.lens (\DescribeKeywordsResponse' {originationIdentity} -> originationIdentity) (\s@DescribeKeywordsResponse' {} a -> s {originationIdentity = a} :: DescribeKeywordsResponse)

-- | The PhoneNumberArn or PoolArn that is associated with the
-- OriginationIdentity.
describeKeywordsResponse_originationIdentityArn :: Lens.Lens' DescribeKeywordsResponse (Prelude.Maybe Prelude.Text)
describeKeywordsResponse_originationIdentityArn = Lens.lens (\DescribeKeywordsResponse' {originationIdentityArn} -> originationIdentityArn) (\s@DescribeKeywordsResponse' {} a -> s {originationIdentityArn = a} :: DescribeKeywordsResponse)

-- | The response's http status code.
describeKeywordsResponse_httpStatus :: Lens.Lens' DescribeKeywordsResponse Prelude.Int
describeKeywordsResponse_httpStatus = Lens.lens (\DescribeKeywordsResponse' {httpStatus} -> httpStatus) (\s@DescribeKeywordsResponse' {} a -> s {httpStatus = a} :: DescribeKeywordsResponse)

instance Prelude.NFData DescribeKeywordsResponse where
  rnf DescribeKeywordsResponse' {..} =
    Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf originationIdentityArn
      `Prelude.seq` Prelude.rnf httpStatus
