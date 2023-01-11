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
-- Module      : Amazonka.APIGateway.FlushStageCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes a stage\'s cache.
module Amazonka.APIGateway.FlushStageCache
  ( -- * Creating a Request
    FlushStageCache (..),
    newFlushStageCache,

    -- * Request Lenses
    flushStageCache_restApiId,
    flushStageCache_stageName,

    -- * Destructuring the Response
    FlushStageCacheResponse (..),
    newFlushStageCacheResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to flush a stage\'s cache.
--
-- /See:/ 'newFlushStageCache' smart constructor.
data FlushStageCache = FlushStageCache'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the stage to flush its cache.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlushStageCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'flushStageCache_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'flushStageCache_stageName' - The name of the stage to flush its cache.
newFlushStageCache ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  FlushStageCache
newFlushStageCache pRestApiId_ pStageName_ =
  FlushStageCache'
    { restApiId = pRestApiId_,
      stageName = pStageName_
    }

-- | The string identifier of the associated RestApi.
flushStageCache_restApiId :: Lens.Lens' FlushStageCache Prelude.Text
flushStageCache_restApiId = Lens.lens (\FlushStageCache' {restApiId} -> restApiId) (\s@FlushStageCache' {} a -> s {restApiId = a} :: FlushStageCache)

-- | The name of the stage to flush its cache.
flushStageCache_stageName :: Lens.Lens' FlushStageCache Prelude.Text
flushStageCache_stageName = Lens.lens (\FlushStageCache' {stageName} -> stageName) (\s@FlushStageCache' {} a -> s {stageName = a} :: FlushStageCache)

instance Core.AWSRequest FlushStageCache where
  type
    AWSResponse FlushStageCache =
      FlushStageCacheResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull FlushStageCacheResponse'

instance Prelude.Hashable FlushStageCache where
  hashWithSalt _salt FlushStageCache' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData FlushStageCache where
  rnf FlushStageCache' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders FlushStageCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath FlushStageCache where
  toPath FlushStageCache' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/stages/",
        Data.toBS stageName,
        "/cache/data"
      ]

instance Data.ToQuery FlushStageCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFlushStageCacheResponse' smart constructor.
data FlushStageCacheResponse = FlushStageCacheResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlushStageCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newFlushStageCacheResponse ::
  FlushStageCacheResponse
newFlushStageCacheResponse = FlushStageCacheResponse'

instance Prelude.NFData FlushStageCacheResponse where
  rnf _ = ()
