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
-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution.
module Network.AWS.CloudFront.UpdateStreamingDistribution
  ( -- * Creating a Request
    UpdateStreamingDistribution (..),
    newUpdateStreamingDistribution,

    -- * Request Lenses
    updateStreamingDistribution_ifMatch,
    updateStreamingDistribution_streamingDistributionConfig,
    updateStreamingDistribution_id,

    -- * Destructuring the Response
    UpdateStreamingDistributionResponse (..),
    newUpdateStreamingDistributionResponse,

    -- * Response Lenses
    updateStreamingDistributionResponse_eTag,
    updateStreamingDistributionResponse_streamingDistribution,
    updateStreamingDistributionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update a streaming distribution.
--
-- /See:/ 'newUpdateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- streaming distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Core.Maybe Core.Text,
    -- | The streaming distribution\'s configuration information.
    streamingDistributionConfig :: StreamingDistributionConfig,
    -- | The streaming distribution\'s id.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStreamingDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateStreamingDistribution_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- streaming distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'streamingDistributionConfig', 'updateStreamingDistribution_streamingDistributionConfig' - The streaming distribution\'s configuration information.
--
-- 'id', 'updateStreamingDistribution_id' - The streaming distribution\'s id.
newUpdateStreamingDistribution ::
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  -- | 'id'
  Core.Text ->
  UpdateStreamingDistribution
newUpdateStreamingDistribution
  pStreamingDistributionConfig_
  pId_ =
    UpdateStreamingDistribution'
      { ifMatch =
          Core.Nothing,
        streamingDistributionConfig =
          pStreamingDistributionConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- streaming distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
updateStreamingDistribution_ifMatch :: Lens.Lens' UpdateStreamingDistribution (Core.Maybe Core.Text)
updateStreamingDistribution_ifMatch = Lens.lens (\UpdateStreamingDistribution' {ifMatch} -> ifMatch) (\s@UpdateStreamingDistribution' {} a -> s {ifMatch = a} :: UpdateStreamingDistribution)

-- | The streaming distribution\'s configuration information.
updateStreamingDistribution_streamingDistributionConfig :: Lens.Lens' UpdateStreamingDistribution StreamingDistributionConfig
updateStreamingDistribution_streamingDistributionConfig = Lens.lens (\UpdateStreamingDistribution' {streamingDistributionConfig} -> streamingDistributionConfig) (\s@UpdateStreamingDistribution' {} a -> s {streamingDistributionConfig = a} :: UpdateStreamingDistribution)

-- | The streaming distribution\'s id.
updateStreamingDistribution_id :: Lens.Lens' UpdateStreamingDistribution Core.Text
updateStreamingDistribution_id = Lens.lens (\UpdateStreamingDistribution' {id} -> id) (\s@UpdateStreamingDistribution' {} a -> s {id = a} :: UpdateStreamingDistribution)

instance Core.AWSRequest UpdateStreamingDistribution where
  type
    AWSResponse UpdateStreamingDistribution =
      UpdateStreamingDistributionResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateStreamingDistributionResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateStreamingDistribution

instance Core.NFData UpdateStreamingDistribution

instance Core.ToElement UpdateStreamingDistribution where
  toElement UpdateStreamingDistribution' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfig"
      streamingDistributionConfig

instance Core.ToHeaders UpdateStreamingDistribution where
  toHeaders UpdateStreamingDistribution' {..} =
    Core.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdateStreamingDistribution where
  toPath UpdateStreamingDistribution' {..} =
    Core.mconcat
      [ "/2020-05-31/streaming-distribution/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery UpdateStreamingDistribution where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The streaming distribution\'s information.
    streamingDistribution :: Core.Maybe StreamingDistribution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStreamingDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateStreamingDistributionResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'streamingDistribution', 'updateStreamingDistributionResponse_streamingDistribution' - The streaming distribution\'s information.
--
-- 'httpStatus', 'updateStreamingDistributionResponse_httpStatus' - The response's http status code.
newUpdateStreamingDistributionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateStreamingDistributionResponse
newUpdateStreamingDistributionResponse pHttpStatus_ =
  UpdateStreamingDistributionResponse'
    { eTag =
        Core.Nothing,
      streamingDistribution = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateStreamingDistributionResponse_eTag :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe Core.Text)
updateStreamingDistributionResponse_eTag = Lens.lens (\UpdateStreamingDistributionResponse' {eTag} -> eTag) (\s@UpdateStreamingDistributionResponse' {} a -> s {eTag = a} :: UpdateStreamingDistributionResponse)

-- | The streaming distribution\'s information.
updateStreamingDistributionResponse_streamingDistribution :: Lens.Lens' UpdateStreamingDistributionResponse (Core.Maybe StreamingDistribution)
updateStreamingDistributionResponse_streamingDistribution = Lens.lens (\UpdateStreamingDistributionResponse' {streamingDistribution} -> streamingDistribution) (\s@UpdateStreamingDistributionResponse' {} a -> s {streamingDistribution = a} :: UpdateStreamingDistributionResponse)

-- | The response's http status code.
updateStreamingDistributionResponse_httpStatus :: Lens.Lens' UpdateStreamingDistributionResponse Core.Int
updateStreamingDistributionResponse_httpStatus = Lens.lens (\UpdateStreamingDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateStreamingDistributionResponse' {} a -> s {httpStatus = a} :: UpdateStreamingDistributionResponse)

instance
  Core.NFData
    UpdateStreamingDistributionResponse
