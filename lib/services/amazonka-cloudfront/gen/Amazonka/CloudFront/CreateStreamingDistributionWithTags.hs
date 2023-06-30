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
-- Module      : Amazonka.CloudFront.CreateStreamingDistributionWithTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is deprecated. Amazon CloudFront is deprecating real-time
-- messaging protocol (RTMP) distributions on December 31, 2020. For more
-- information,
-- <http://forums.aws.amazon.com/ann.jspa?annID=7356 read the announcement>
-- on the Amazon CloudFront discussion forum.
module Amazonka.CloudFront.CreateStreamingDistributionWithTags
  ( -- * Creating a Request
    CreateStreamingDistributionWithTags (..),
    newCreateStreamingDistributionWithTags,

    -- * Request Lenses
    createStreamingDistributionWithTags_streamingDistributionConfigWithTags,

    -- * Destructuring the Response
    CreateStreamingDistributionWithTagsResponse (..),
    newCreateStreamingDistributionWithTagsResponse,

    -- * Response Lenses
    createStreamingDistributionWithTagsResponse_eTag,
    createStreamingDistributionWithTagsResponse_location,
    createStreamingDistributionWithTagsResponse_streamingDistribution,
    createStreamingDistributionWithTagsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to create a new streaming distribution with tags.
--
-- /See:/ 'newCreateStreamingDistributionWithTags' smart constructor.
data CreateStreamingDistributionWithTags = CreateStreamingDistributionWithTags'
  { -- | The streaming distribution\'s configuration information.
    streamingDistributionConfigWithTags :: StreamingDistributionConfigWithTags
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingDistributionWithTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingDistributionConfigWithTags', 'createStreamingDistributionWithTags_streamingDistributionConfigWithTags' - The streaming distribution\'s configuration information.
newCreateStreamingDistributionWithTags ::
  -- | 'streamingDistributionConfigWithTags'
  StreamingDistributionConfigWithTags ->
  CreateStreamingDistributionWithTags
newCreateStreamingDistributionWithTags
  pStreamingDistributionConfigWithTags_ =
    CreateStreamingDistributionWithTags'
      { streamingDistributionConfigWithTags =
          pStreamingDistributionConfigWithTags_
      }

-- | The streaming distribution\'s configuration information.
createStreamingDistributionWithTags_streamingDistributionConfigWithTags :: Lens.Lens' CreateStreamingDistributionWithTags StreamingDistributionConfigWithTags
createStreamingDistributionWithTags_streamingDistributionConfigWithTags = Lens.lens (\CreateStreamingDistributionWithTags' {streamingDistributionConfigWithTags} -> streamingDistributionConfigWithTags) (\s@CreateStreamingDistributionWithTags' {} a -> s {streamingDistributionConfigWithTags = a} :: CreateStreamingDistributionWithTags)

instance
  Core.AWSRequest
    CreateStreamingDistributionWithTags
  where
  type
    AWSResponse CreateStreamingDistributionWithTags =
      CreateStreamingDistributionWithTagsResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateStreamingDistributionWithTagsResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateStreamingDistributionWithTags
  where
  hashWithSalt
    _salt
    CreateStreamingDistributionWithTags' {..} =
      _salt
        `Prelude.hashWithSalt` streamingDistributionConfigWithTags

instance
  Prelude.NFData
    CreateStreamingDistributionWithTags
  where
  rnf CreateStreamingDistributionWithTags' {..} =
    Prelude.rnf streamingDistributionConfigWithTags

instance
  Data.ToElement
    CreateStreamingDistributionWithTags
  where
  toElement CreateStreamingDistributionWithTags' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfigWithTags"
      streamingDistributionConfigWithTags

instance
  Data.ToHeaders
    CreateStreamingDistributionWithTags
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateStreamingDistributionWithTags
  where
  toPath =
    Prelude.const "/2020-05-31/streaming-distribution"

instance
  Data.ToQuery
    CreateStreamingDistributionWithTags
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateStreamingDistributionWithTagsResponse' smart constructor.
data CreateStreamingDistributionWithTagsResponse = CreateStreamingDistributionWithTagsResponse'
  { -- | The current version of the distribution created.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified URI of the new streaming distribution resource just
    -- created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The streaming distribution\'s information.
    streamingDistribution :: Prelude.Maybe StreamingDistribution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingDistributionWithTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createStreamingDistributionWithTagsResponse_eTag' - The current version of the distribution created.
--
-- 'location', 'createStreamingDistributionWithTagsResponse_location' - The fully qualified URI of the new streaming distribution resource just
-- created.
--
-- 'streamingDistribution', 'createStreamingDistributionWithTagsResponse_streamingDistribution' - The streaming distribution\'s information.
--
-- 'httpStatus', 'createStreamingDistributionWithTagsResponse_httpStatus' - The response's http status code.
newCreateStreamingDistributionWithTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamingDistributionWithTagsResponse
newCreateStreamingDistributionWithTagsResponse
  pHttpStatus_ =
    CreateStreamingDistributionWithTagsResponse'
      { eTag =
          Prelude.Nothing,
        location = Prelude.Nothing,
        streamingDistribution =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the distribution created.
createStreamingDistributionWithTagsResponse_eTag :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createStreamingDistributionWithTagsResponse_eTag = Lens.lens (\CreateStreamingDistributionWithTagsResponse' {eTag} -> eTag) (\s@CreateStreamingDistributionWithTagsResponse' {} a -> s {eTag = a} :: CreateStreamingDistributionWithTagsResponse)

-- | The fully qualified URI of the new streaming distribution resource just
-- created.
createStreamingDistributionWithTagsResponse_location :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createStreamingDistributionWithTagsResponse_location = Lens.lens (\CreateStreamingDistributionWithTagsResponse' {location} -> location) (\s@CreateStreamingDistributionWithTagsResponse' {} a -> s {location = a} :: CreateStreamingDistributionWithTagsResponse)

-- | The streaming distribution\'s information.
createStreamingDistributionWithTagsResponse_streamingDistribution :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Prelude.Maybe StreamingDistribution)
createStreamingDistributionWithTagsResponse_streamingDistribution = Lens.lens (\CreateStreamingDistributionWithTagsResponse' {streamingDistribution} -> streamingDistribution) (\s@CreateStreamingDistributionWithTagsResponse' {} a -> s {streamingDistribution = a} :: CreateStreamingDistributionWithTagsResponse)

-- | The response's http status code.
createStreamingDistributionWithTagsResponse_httpStatus :: Lens.Lens' CreateStreamingDistributionWithTagsResponse Prelude.Int
createStreamingDistributionWithTagsResponse_httpStatus = Lens.lens (\CreateStreamingDistributionWithTagsResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingDistributionWithTagsResponse' {} a -> s {httpStatus = a} :: CreateStreamingDistributionWithTagsResponse)

instance
  Prelude.NFData
    CreateStreamingDistributionWithTagsResponse
  where
  rnf CreateStreamingDistributionWithTagsResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf streamingDistribution
      `Prelude.seq` Prelude.rnf httpStatus
