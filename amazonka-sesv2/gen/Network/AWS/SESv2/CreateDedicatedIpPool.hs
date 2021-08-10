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
-- Module      : Network.AWS.SESv2.CreateDedicatedIpPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new pool of dedicated IP addresses. A pool can include one or
-- more dedicated IP addresses that are associated with your AWS account.
-- You can associate a pool with a configuration set. When you send an
-- email that uses that configuration set, the message is sent from one of
-- the addresses in the associated pool.
module Network.AWS.SESv2.CreateDedicatedIpPool
  ( -- * Creating a Request
    CreateDedicatedIpPool (..),
    newCreateDedicatedIpPool,

    -- * Request Lenses
    createDedicatedIpPool_tags,
    createDedicatedIpPool_poolName,

    -- * Destructuring the Response
    CreateDedicatedIpPoolResponse (..),
    newCreateDedicatedIpPoolResponse,

    -- * Response Lenses
    createDedicatedIpPoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to create a new dedicated IP pool.
--
-- /See:/ 'newCreateDedicatedIpPool' smart constructor.
data CreateDedicatedIpPool = CreateDedicatedIpPool'
  { -- | An object that defines the tags (keys and values) that you want to
    -- associate with the pool.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the dedicated IP pool.
    poolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDedicatedIpPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDedicatedIpPool_tags' - An object that defines the tags (keys and values) that you want to
-- associate with the pool.
--
-- 'poolName', 'createDedicatedIpPool_poolName' - The name of the dedicated IP pool.
newCreateDedicatedIpPool ::
  -- | 'poolName'
  Prelude.Text ->
  CreateDedicatedIpPool
newCreateDedicatedIpPool pPoolName_ =
  CreateDedicatedIpPool'
    { tags = Prelude.Nothing,
      poolName = pPoolName_
    }

-- | An object that defines the tags (keys and values) that you want to
-- associate with the pool.
createDedicatedIpPool_tags :: Lens.Lens' CreateDedicatedIpPool (Prelude.Maybe [Tag])
createDedicatedIpPool_tags = Lens.lens (\CreateDedicatedIpPool' {tags} -> tags) (\s@CreateDedicatedIpPool' {} a -> s {tags = a} :: CreateDedicatedIpPool) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the dedicated IP pool.
createDedicatedIpPool_poolName :: Lens.Lens' CreateDedicatedIpPool Prelude.Text
createDedicatedIpPool_poolName = Lens.lens (\CreateDedicatedIpPool' {poolName} -> poolName) (\s@CreateDedicatedIpPool' {} a -> s {poolName = a} :: CreateDedicatedIpPool)

instance Core.AWSRequest CreateDedicatedIpPool where
  type
    AWSResponse CreateDedicatedIpPool =
      CreateDedicatedIpPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDedicatedIpPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDedicatedIpPool

instance Prelude.NFData CreateDedicatedIpPool

instance Core.ToHeaders CreateDedicatedIpPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDedicatedIpPool where
  toJSON CreateDedicatedIpPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("PoolName" Core..= poolName)
          ]
      )

instance Core.ToPath CreateDedicatedIpPool where
  toPath = Prelude.const "/v2/email/dedicated-ip-pools"

instance Core.ToQuery CreateDedicatedIpPool where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newCreateDedicatedIpPoolResponse' smart constructor.
data CreateDedicatedIpPoolResponse = CreateDedicatedIpPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDedicatedIpPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDedicatedIpPoolResponse_httpStatus' - The response's http status code.
newCreateDedicatedIpPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDedicatedIpPoolResponse
newCreateDedicatedIpPoolResponse pHttpStatus_ =
  CreateDedicatedIpPoolResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createDedicatedIpPoolResponse_httpStatus :: Lens.Lens' CreateDedicatedIpPoolResponse Prelude.Int
createDedicatedIpPoolResponse_httpStatus = Lens.lens (\CreateDedicatedIpPoolResponse' {httpStatus} -> httpStatus) (\s@CreateDedicatedIpPoolResponse' {} a -> s {httpStatus = a} :: CreateDedicatedIpPoolResponse)

instance Prelude.NFData CreateDedicatedIpPoolResponse
