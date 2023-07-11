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
-- Module      : Amazonka.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified RTMP distribution, including the
-- distribution configuration.
module Amazonka.CloudFront.GetStreamingDistribution
  ( -- * Creating a Request
    GetStreamingDistribution (..),
    newGetStreamingDistribution,

    -- * Request Lenses
    getStreamingDistribution_id,

    -- * Destructuring the Response
    GetStreamingDistributionResponse (..),
    newGetStreamingDistributionResponse,

    -- * Response Lenses
    getStreamingDistributionResponse_eTag,
    getStreamingDistributionResponse_streamingDistribution,
    getStreamingDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to get a streaming distribution\'s information.
--
-- /See:/ 'newGetStreamingDistribution' smart constructor.
data GetStreamingDistribution = GetStreamingDistribution'
  { -- | The streaming distribution\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getStreamingDistribution_id' - The streaming distribution\'s ID.
newGetStreamingDistribution ::
  -- | 'id'
  Prelude.Text ->
  GetStreamingDistribution
newGetStreamingDistribution pId_ =
  GetStreamingDistribution' {id = pId_}

-- | The streaming distribution\'s ID.
getStreamingDistribution_id :: Lens.Lens' GetStreamingDistribution Prelude.Text
getStreamingDistribution_id = Lens.lens (\GetStreamingDistribution' {id} -> id) (\s@GetStreamingDistribution' {} a -> s {id = a} :: GetStreamingDistribution)

instance Core.AWSRequest GetStreamingDistribution where
  type
    AWSResponse GetStreamingDistribution =
      GetStreamingDistributionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetStreamingDistributionResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingDistribution where
  hashWithSalt _salt GetStreamingDistribution' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetStreamingDistribution where
  rnf GetStreamingDistribution' {..} = Prelude.rnf id

instance Data.ToHeaders GetStreamingDistribution where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetStreamingDistribution where
  toPath GetStreamingDistribution' {..} =
    Prelude.mconcat
      ["/2020-05-31/streaming-distribution/", Data.toBS id]

instance Data.ToQuery GetStreamingDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetStreamingDistributionResponse' smart constructor.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
  { -- | The current version of the streaming distribution\'s information. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The streaming distribution\'s information.
    streamingDistribution :: Prelude.Maybe StreamingDistribution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getStreamingDistributionResponse_eTag' - The current version of the streaming distribution\'s information. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'streamingDistribution', 'getStreamingDistributionResponse_streamingDistribution' - The streaming distribution\'s information.
--
-- 'httpStatus', 'getStreamingDistributionResponse_httpStatus' - The response's http status code.
newGetStreamingDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingDistributionResponse
newGetStreamingDistributionResponse pHttpStatus_ =
  GetStreamingDistributionResponse'
    { eTag =
        Prelude.Nothing,
      streamingDistribution = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the streaming distribution\'s information. For
-- example: @E2QWRUHAPOMQZL@.
getStreamingDistributionResponse_eTag :: Lens.Lens' GetStreamingDistributionResponse (Prelude.Maybe Prelude.Text)
getStreamingDistributionResponse_eTag = Lens.lens (\GetStreamingDistributionResponse' {eTag} -> eTag) (\s@GetStreamingDistributionResponse' {} a -> s {eTag = a} :: GetStreamingDistributionResponse)

-- | The streaming distribution\'s information.
getStreamingDistributionResponse_streamingDistribution :: Lens.Lens' GetStreamingDistributionResponse (Prelude.Maybe StreamingDistribution)
getStreamingDistributionResponse_streamingDistribution = Lens.lens (\GetStreamingDistributionResponse' {streamingDistribution} -> streamingDistribution) (\s@GetStreamingDistributionResponse' {} a -> s {streamingDistribution = a} :: GetStreamingDistributionResponse)

-- | The response's http status code.
getStreamingDistributionResponse_httpStatus :: Lens.Lens' GetStreamingDistributionResponse Prelude.Int
getStreamingDistributionResponse_httpStatus = Lens.lens (\GetStreamingDistributionResponse' {httpStatus} -> httpStatus) (\s@GetStreamingDistributionResponse' {} a -> s {httpStatus = a} :: GetStreamingDistributionResponse)

instance
  Prelude.NFData
    GetStreamingDistributionResponse
  where
  rnf GetStreamingDistributionResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf streamingDistribution
      `Prelude.seq` Prelude.rnf httpStatus
