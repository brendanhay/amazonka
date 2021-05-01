{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.FlushStageCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes a stage\'s cache.
module Network.AWS.APIGateway.FlushStageCache
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to flush a stage\'s cache.
--
-- /See:/ 'newFlushStageCache' smart constructor.
data FlushStageCache = FlushStageCache'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the stage to flush its cache.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushStageCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'flushStageCache_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'stageName', 'flushStageCache_stageName' - [Required] The name of the stage to flush its cache.
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

-- | [Required] The string identifier of the associated RestApi.
flushStageCache_restApiId :: Lens.Lens' FlushStageCache Prelude.Text
flushStageCache_restApiId = Lens.lens (\FlushStageCache' {restApiId} -> restApiId) (\s@FlushStageCache' {} a -> s {restApiId = a} :: FlushStageCache)

-- | [Required] The name of the stage to flush its cache.
flushStageCache_stageName :: Lens.Lens' FlushStageCache Prelude.Text
flushStageCache_stageName = Lens.lens (\FlushStageCache' {stageName} -> stageName) (\s@FlushStageCache' {} a -> s {stageName = a} :: FlushStageCache)

instance Prelude.AWSRequest FlushStageCache where
  type Rs FlushStageCache = FlushStageCacheResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull FlushStageCacheResponse'

instance Prelude.Hashable FlushStageCache

instance Prelude.NFData FlushStageCache

instance Prelude.ToHeaders FlushStageCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath FlushStageCache where
  toPath FlushStageCache' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/stages/",
        Prelude.toBS stageName,
        "/cache/data"
      ]

instance Prelude.ToQuery FlushStageCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFlushStageCacheResponse' smart constructor.
data FlushStageCacheResponse = FlushStageCacheResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushStageCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newFlushStageCacheResponse ::
  FlushStageCacheResponse
newFlushStageCacheResponse = FlushStageCacheResponse'

instance Prelude.NFData FlushStageCacheResponse
