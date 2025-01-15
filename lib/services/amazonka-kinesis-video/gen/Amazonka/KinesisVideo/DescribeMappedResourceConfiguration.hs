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
-- Module      : Amazonka.KinesisVideo.DescribeMappedResourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the stream. Either streamName
-- or streamARN should be provided in the input.
--
-- Returns the most current information about the stream. The @streamName@
-- or @streamARN@ should be provided in the input.
--
-- This operation returns paginated results.
module Amazonka.KinesisVideo.DescribeMappedResourceConfiguration
  ( -- * Creating a Request
    DescribeMappedResourceConfiguration (..),
    newDescribeMappedResourceConfiguration,

    -- * Request Lenses
    describeMappedResourceConfiguration_maxResults,
    describeMappedResourceConfiguration_nextToken,
    describeMappedResourceConfiguration_streamARN,
    describeMappedResourceConfiguration_streamName,

    -- * Destructuring the Response
    DescribeMappedResourceConfigurationResponse (..),
    newDescribeMappedResourceConfigurationResponse,

    -- * Response Lenses
    describeMappedResourceConfigurationResponse_mappedResourceConfigurationList,
    describeMappedResourceConfigurationResponse_nextToken,
    describeMappedResourceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMappedResourceConfiguration' smart constructor.
data DescribeMappedResourceConfiguration = DescribeMappedResourceConfiguration'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to provide in your next request, to get another batch of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMappedResourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeMappedResourceConfiguration_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'describeMappedResourceConfiguration_nextToken' - The token to provide in your next request, to get another batch of
-- results.
--
-- 'streamARN', 'describeMappedResourceConfiguration_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'describeMappedResourceConfiguration_streamName' - The name of the stream.
newDescribeMappedResourceConfiguration ::
  DescribeMappedResourceConfiguration
newDescribeMappedResourceConfiguration =
  DescribeMappedResourceConfiguration'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The maximum number of results to return in the response.
describeMappedResourceConfiguration_maxResults :: Lens.Lens' DescribeMappedResourceConfiguration (Prelude.Maybe Prelude.Natural)
describeMappedResourceConfiguration_maxResults = Lens.lens (\DescribeMappedResourceConfiguration' {maxResults} -> maxResults) (\s@DescribeMappedResourceConfiguration' {} a -> s {maxResults = a} :: DescribeMappedResourceConfiguration)

-- | The token to provide in your next request, to get another batch of
-- results.
describeMappedResourceConfiguration_nextToken :: Lens.Lens' DescribeMappedResourceConfiguration (Prelude.Maybe Prelude.Text)
describeMappedResourceConfiguration_nextToken = Lens.lens (\DescribeMappedResourceConfiguration' {nextToken} -> nextToken) (\s@DescribeMappedResourceConfiguration' {} a -> s {nextToken = a} :: DescribeMappedResourceConfiguration)

-- | The Amazon Resource Name (ARN) of the stream.
describeMappedResourceConfiguration_streamARN :: Lens.Lens' DescribeMappedResourceConfiguration (Prelude.Maybe Prelude.Text)
describeMappedResourceConfiguration_streamARN = Lens.lens (\DescribeMappedResourceConfiguration' {streamARN} -> streamARN) (\s@DescribeMappedResourceConfiguration' {} a -> s {streamARN = a} :: DescribeMappedResourceConfiguration)

-- | The name of the stream.
describeMappedResourceConfiguration_streamName :: Lens.Lens' DescribeMappedResourceConfiguration (Prelude.Maybe Prelude.Text)
describeMappedResourceConfiguration_streamName = Lens.lens (\DescribeMappedResourceConfiguration' {streamName} -> streamName) (\s@DescribeMappedResourceConfiguration' {} a -> s {streamName = a} :: DescribeMappedResourceConfiguration)

instance
  Core.AWSPager
    DescribeMappedResourceConfiguration
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMappedResourceConfigurationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMappedResourceConfigurationResponse_mappedResourceConfigurationList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeMappedResourceConfiguration_nextToken
              Lens..~ rs
              Lens.^? describeMappedResourceConfigurationResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMappedResourceConfiguration
  where
  type
    AWSResponse DescribeMappedResourceConfiguration =
      DescribeMappedResourceConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMappedResourceConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "MappedResourceConfigurationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMappedResourceConfiguration
  where
  hashWithSalt
    _salt
    DescribeMappedResourceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` streamARN
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    DescribeMappedResourceConfiguration
  where
  rnf DescribeMappedResourceConfiguration' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf streamARN `Prelude.seq`
          Prelude.rnf streamName

instance
  Data.ToHeaders
    DescribeMappedResourceConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DescribeMappedResourceConfiguration
  where
  toJSON DescribeMappedResourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance
  Data.ToPath
    DescribeMappedResourceConfiguration
  where
  toPath =
    Prelude.const
      "/describeMappedResourceConfiguration"

instance
  Data.ToQuery
    DescribeMappedResourceConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMappedResourceConfigurationResponse' smart constructor.
data DescribeMappedResourceConfigurationResponse = DescribeMappedResourceConfigurationResponse'
  { -- | A structure that encapsulates, or contains, the media storage
    -- configuration properties.
    mappedResourceConfigurationList :: Prelude.Maybe [MappedResourceConfigurationListItem],
    -- | The token that was used in the @NextToken@request to fetch the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMappedResourceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mappedResourceConfigurationList', 'describeMappedResourceConfigurationResponse_mappedResourceConfigurationList' - A structure that encapsulates, or contains, the media storage
-- configuration properties.
--
-- 'nextToken', 'describeMappedResourceConfigurationResponse_nextToken' - The token that was used in the @NextToken@request to fetch the next set
-- of results.
--
-- 'httpStatus', 'describeMappedResourceConfigurationResponse_httpStatus' - The response's http status code.
newDescribeMappedResourceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMappedResourceConfigurationResponse
newDescribeMappedResourceConfigurationResponse
  pHttpStatus_ =
    DescribeMappedResourceConfigurationResponse'
      { mappedResourceConfigurationList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A structure that encapsulates, or contains, the media storage
-- configuration properties.
describeMappedResourceConfigurationResponse_mappedResourceConfigurationList :: Lens.Lens' DescribeMappedResourceConfigurationResponse (Prelude.Maybe [MappedResourceConfigurationListItem])
describeMappedResourceConfigurationResponse_mappedResourceConfigurationList = Lens.lens (\DescribeMappedResourceConfigurationResponse' {mappedResourceConfigurationList} -> mappedResourceConfigurationList) (\s@DescribeMappedResourceConfigurationResponse' {} a -> s {mappedResourceConfigurationList = a} :: DescribeMappedResourceConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that was used in the @NextToken@request to fetch the next set
-- of results.
describeMappedResourceConfigurationResponse_nextToken :: Lens.Lens' DescribeMappedResourceConfigurationResponse (Prelude.Maybe Prelude.Text)
describeMappedResourceConfigurationResponse_nextToken = Lens.lens (\DescribeMappedResourceConfigurationResponse' {nextToken} -> nextToken) (\s@DescribeMappedResourceConfigurationResponse' {} a -> s {nextToken = a} :: DescribeMappedResourceConfigurationResponse)

-- | The response's http status code.
describeMappedResourceConfigurationResponse_httpStatus :: Lens.Lens' DescribeMappedResourceConfigurationResponse Prelude.Int
describeMappedResourceConfigurationResponse_httpStatus = Lens.lens (\DescribeMappedResourceConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeMappedResourceConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeMappedResourceConfigurationResponse)

instance
  Prelude.NFData
    DescribeMappedResourceConfigurationResponse
  where
  rnf DescribeMappedResourceConfigurationResponse' {..} =
    Prelude.rnf mappedResourceConfigurationList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
