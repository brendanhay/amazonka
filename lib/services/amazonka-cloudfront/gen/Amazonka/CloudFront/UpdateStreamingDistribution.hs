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
-- Module      : Amazonka.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution.
module Amazonka.CloudFront.UpdateStreamingDistribution
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
    updateStreamingDistributionResponse_streamingDistribution,
    updateStreamingDistributionResponse_eTag,
    updateStreamingDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to update a streaming distribution.
--
-- /See:/ 'newUpdateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- streaming distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The streaming distribution\'s configuration information.
    streamingDistributionConfig :: StreamingDistributionConfig,
    -- | The streaming distribution\'s id.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateStreamingDistribution
newUpdateStreamingDistribution
  pStreamingDistributionConfig_
  pId_ =
    UpdateStreamingDistribution'
      { ifMatch =
          Prelude.Nothing,
        streamingDistributionConfig =
          pStreamingDistributionConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- streaming distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
updateStreamingDistribution_ifMatch :: Lens.Lens' UpdateStreamingDistribution (Prelude.Maybe Prelude.Text)
updateStreamingDistribution_ifMatch = Lens.lens (\UpdateStreamingDistribution' {ifMatch} -> ifMatch) (\s@UpdateStreamingDistribution' {} a -> s {ifMatch = a} :: UpdateStreamingDistribution)

-- | The streaming distribution\'s configuration information.
updateStreamingDistribution_streamingDistributionConfig :: Lens.Lens' UpdateStreamingDistribution StreamingDistributionConfig
updateStreamingDistribution_streamingDistributionConfig = Lens.lens (\UpdateStreamingDistribution' {streamingDistributionConfig} -> streamingDistributionConfig) (\s@UpdateStreamingDistribution' {} a -> s {streamingDistributionConfig = a} :: UpdateStreamingDistribution)

-- | The streaming distribution\'s id.
updateStreamingDistribution_id :: Lens.Lens' UpdateStreamingDistribution Prelude.Text
updateStreamingDistribution_id = Lens.lens (\UpdateStreamingDistribution' {id} -> id) (\s@UpdateStreamingDistribution' {} a -> s {id = a} :: UpdateStreamingDistribution)

instance Core.AWSRequest UpdateStreamingDistribution where
  type
    AWSResponse UpdateStreamingDistribution =
      UpdateStreamingDistributionResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateStreamingDistributionResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStreamingDistribution where
  hashWithSalt _salt UpdateStreamingDistribution' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` streamingDistributionConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateStreamingDistribution where
  rnf UpdateStreamingDistribution' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf streamingDistributionConfig
      `Prelude.seq` Prelude.rnf id

instance Core.ToElement UpdateStreamingDistribution where
  toElement UpdateStreamingDistribution' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfig"
      streamingDistributionConfig

instance Core.ToHeaders UpdateStreamingDistribution where
  toHeaders UpdateStreamingDistribution' {..} =
    Prelude.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdateStreamingDistribution where
  toPath UpdateStreamingDistribution' {..} =
    Prelude.mconcat
      [ "/2020-05-31/streaming-distribution/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery UpdateStreamingDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { -- | The streaming distribution\'s information.
    streamingDistribution :: Prelude.Maybe StreamingDistribution,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamingDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingDistribution', 'updateStreamingDistributionResponse_streamingDistribution' - The streaming distribution\'s information.
--
-- 'eTag', 'updateStreamingDistributionResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'updateStreamingDistributionResponse_httpStatus' - The response's http status code.
newUpdateStreamingDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStreamingDistributionResponse
newUpdateStreamingDistributionResponse pHttpStatus_ =
  UpdateStreamingDistributionResponse'
    { streamingDistribution =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The streaming distribution\'s information.
updateStreamingDistributionResponse_streamingDistribution :: Lens.Lens' UpdateStreamingDistributionResponse (Prelude.Maybe StreamingDistribution)
updateStreamingDistributionResponse_streamingDistribution = Lens.lens (\UpdateStreamingDistributionResponse' {streamingDistribution} -> streamingDistribution) (\s@UpdateStreamingDistributionResponse' {} a -> s {streamingDistribution = a} :: UpdateStreamingDistributionResponse)

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateStreamingDistributionResponse_eTag :: Lens.Lens' UpdateStreamingDistributionResponse (Prelude.Maybe Prelude.Text)
updateStreamingDistributionResponse_eTag = Lens.lens (\UpdateStreamingDistributionResponse' {eTag} -> eTag) (\s@UpdateStreamingDistributionResponse' {} a -> s {eTag = a} :: UpdateStreamingDistributionResponse)

-- | The response's http status code.
updateStreamingDistributionResponse_httpStatus :: Lens.Lens' UpdateStreamingDistributionResponse Prelude.Int
updateStreamingDistributionResponse_httpStatus = Lens.lens (\UpdateStreamingDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateStreamingDistributionResponse' {} a -> s {httpStatus = a} :: UpdateStreamingDistributionResponse)

instance
  Prelude.NFData
    UpdateStreamingDistributionResponse
  where
  rnf UpdateStreamingDistributionResponse' {..} =
    Prelude.rnf streamingDistribution
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
