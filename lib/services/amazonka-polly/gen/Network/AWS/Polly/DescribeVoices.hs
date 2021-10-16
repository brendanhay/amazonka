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
-- Module      : Network.AWS.Polly.DescribeVoices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of voices that are available for use when requesting
-- speech synthesis. Each voice speaks a specified language, is either male
-- or female, and is identified by an ID, which is the ASCII version of the
-- voice name.
--
-- When synthesizing speech ( @SynthesizeSpeech@ ), you provide the voice
-- ID for the voice you want from the list of voices returned by
-- @DescribeVoices@.
--
-- For example, you want your news reader application to read news in a
-- specific language, but giving a user the option to choose the voice.
-- Using the @DescribeVoices@ operation you can provide the user with a
-- list of available voices to select from.
--
-- You can optionally specify a language code to filter the available
-- voices. For example, if you specify @en-US@, the operation returns a
-- list of all available US English voices.
--
-- This operation requires permissions to perform the
-- @polly:DescribeVoices@ action.
--
-- This operation returns paginated results.
module Network.AWS.Polly.DescribeVoices
  ( -- * Creating a Request
    DescribeVoices (..),
    newDescribeVoices,

    -- * Request Lenses
    describeVoices_languageCode,
    describeVoices_nextToken,
    describeVoices_includeAdditionalLanguageCodes,
    describeVoices_engine,

    -- * Destructuring the Response
    DescribeVoicesResponse (..),
    newDescribeVoicesResponse,

    -- * Response Lenses
    describeVoicesResponse_nextToken,
    describeVoicesResponse_voices,
    describeVoicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVoices' smart constructor.
data DescribeVoices = DescribeVoices'
  { -- | The language identification tag (ISO 639 code for the language name-ISO
    -- 3166 country code) for filtering the list of voices returned. If you
    -- don\'t specify this optional parameter, all available voices are
    -- returned.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | An opaque pagination token returned from the previous @DescribeVoices@
    -- operation. If present, this indicates where to continue the listing.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Boolean value indicating whether to return any bilingual voices that use
    -- the specified language as an additional language. For instance, if you
    -- request all languages that use US English (es-US), and there is an
    -- Italian voice that speaks both Italian (it-IT) and US English, that
    -- voice will be included if you specify @yes@ but not if you specify @no@.
    includeAdditionalLanguageCodes :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the engine (@standard@ or @neural@) used by Amazon Polly when
    -- processing input text for speech synthesis.
    engine :: Prelude.Maybe Engine
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVoices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'describeVoices_languageCode' - The language identification tag (ISO 639 code for the language name-ISO
-- 3166 country code) for filtering the list of voices returned. If you
-- don\'t specify this optional parameter, all available voices are
-- returned.
--
-- 'nextToken', 'describeVoices_nextToken' - An opaque pagination token returned from the previous @DescribeVoices@
-- operation. If present, this indicates where to continue the listing.
--
-- 'includeAdditionalLanguageCodes', 'describeVoices_includeAdditionalLanguageCodes' - Boolean value indicating whether to return any bilingual voices that use
-- the specified language as an additional language. For instance, if you
-- request all languages that use US English (es-US), and there is an
-- Italian voice that speaks both Italian (it-IT) and US English, that
-- voice will be included if you specify @yes@ but not if you specify @no@.
--
-- 'engine', 'describeVoices_engine' - Specifies the engine (@standard@ or @neural@) used by Amazon Polly when
-- processing input text for speech synthesis.
newDescribeVoices ::
  DescribeVoices
newDescribeVoices =
  DescribeVoices'
    { languageCode = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      includeAdditionalLanguageCodes = Prelude.Nothing,
      engine = Prelude.Nothing
    }

-- | The language identification tag (ISO 639 code for the language name-ISO
-- 3166 country code) for filtering the list of voices returned. If you
-- don\'t specify this optional parameter, all available voices are
-- returned.
describeVoices_languageCode :: Lens.Lens' DescribeVoices (Prelude.Maybe LanguageCode)
describeVoices_languageCode = Lens.lens (\DescribeVoices' {languageCode} -> languageCode) (\s@DescribeVoices' {} a -> s {languageCode = a} :: DescribeVoices)

-- | An opaque pagination token returned from the previous @DescribeVoices@
-- operation. If present, this indicates where to continue the listing.
describeVoices_nextToken :: Lens.Lens' DescribeVoices (Prelude.Maybe Prelude.Text)
describeVoices_nextToken = Lens.lens (\DescribeVoices' {nextToken} -> nextToken) (\s@DescribeVoices' {} a -> s {nextToken = a} :: DescribeVoices)

-- | Boolean value indicating whether to return any bilingual voices that use
-- the specified language as an additional language. For instance, if you
-- request all languages that use US English (es-US), and there is an
-- Italian voice that speaks both Italian (it-IT) and US English, that
-- voice will be included if you specify @yes@ but not if you specify @no@.
describeVoices_includeAdditionalLanguageCodes :: Lens.Lens' DescribeVoices (Prelude.Maybe Prelude.Bool)
describeVoices_includeAdditionalLanguageCodes = Lens.lens (\DescribeVoices' {includeAdditionalLanguageCodes} -> includeAdditionalLanguageCodes) (\s@DescribeVoices' {} a -> s {includeAdditionalLanguageCodes = a} :: DescribeVoices)

-- | Specifies the engine (@standard@ or @neural@) used by Amazon Polly when
-- processing input text for speech synthesis.
describeVoices_engine :: Lens.Lens' DescribeVoices (Prelude.Maybe Engine)
describeVoices_engine = Lens.lens (\DescribeVoices' {engine} -> engine) (\s@DescribeVoices' {} a -> s {engine = a} :: DescribeVoices)

instance Core.AWSPager DescribeVoices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVoicesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVoicesResponse_voices Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVoices_nextToken
          Lens..~ rs
          Lens.^? describeVoicesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeVoices where
  type
    AWSResponse DescribeVoices =
      DescribeVoicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVoicesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Voices" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVoices

instance Prelude.NFData DescribeVoices

instance Core.ToHeaders DescribeVoices where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeVoices where
  toPath = Prelude.const "/v1/voices"

instance Core.ToQuery DescribeVoices where
  toQuery DescribeVoices' {..} =
    Prelude.mconcat
      [ "LanguageCode" Core.=: languageCode,
        "NextToken" Core.=: nextToken,
        "IncludeAdditionalLanguageCodes"
          Core.=: includeAdditionalLanguageCodes,
        "Engine" Core.=: engine
      ]

-- | /See:/ 'newDescribeVoicesResponse' smart constructor.
data DescribeVoicesResponse = DescribeVoicesResponse'
  { -- | The pagination token to use in the next request to continue the listing
    -- of voices. @NextToken@ is returned only if the response is truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of voices with their properties.
    voices :: Prelude.Maybe [Voice],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVoicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVoicesResponse_nextToken' - The pagination token to use in the next request to continue the listing
-- of voices. @NextToken@ is returned only if the response is truncated.
--
-- 'voices', 'describeVoicesResponse_voices' - A list of voices with their properties.
--
-- 'httpStatus', 'describeVoicesResponse_httpStatus' - The response's http status code.
newDescribeVoicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVoicesResponse
newDescribeVoicesResponse pHttpStatus_ =
  DescribeVoicesResponse'
    { nextToken =
        Prelude.Nothing,
      voices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use in the next request to continue the listing
-- of voices. @NextToken@ is returned only if the response is truncated.
describeVoicesResponse_nextToken :: Lens.Lens' DescribeVoicesResponse (Prelude.Maybe Prelude.Text)
describeVoicesResponse_nextToken = Lens.lens (\DescribeVoicesResponse' {nextToken} -> nextToken) (\s@DescribeVoicesResponse' {} a -> s {nextToken = a} :: DescribeVoicesResponse)

-- | A list of voices with their properties.
describeVoicesResponse_voices :: Lens.Lens' DescribeVoicesResponse (Prelude.Maybe [Voice])
describeVoicesResponse_voices = Lens.lens (\DescribeVoicesResponse' {voices} -> voices) (\s@DescribeVoicesResponse' {} a -> s {voices = a} :: DescribeVoicesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVoicesResponse_httpStatus :: Lens.Lens' DescribeVoicesResponse Prelude.Int
describeVoicesResponse_httpStatus = Lens.lens (\DescribeVoicesResponse' {httpStatus} -> httpStatus) (\s@DescribeVoicesResponse' {} a -> s {httpStatus = a} :: DescribeVoicesResponse)

instance Prelude.NFData DescribeVoicesResponse
