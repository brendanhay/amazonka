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
-- Module      : Amazonka.PinpointEmail.CreateDedicatedIpPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new pool of dedicated IP addresses. A pool can include one or
-- more dedicated IP addresses that are associated with your Amazon
-- Pinpoint account. You can associate a pool with a configuration set.
-- When you send an email that uses that configuration set, Amazon Pinpoint
-- sends it using only the IP addresses in the associated pool.
module Amazonka.PinpointEmail.CreateDedicatedIpPool
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
createDedicatedIpPool_tags = Lens.lens (\CreateDedicatedIpPool' {tags} -> tags) (\s@CreateDedicatedIpPool' {} a -> s {tags = a} :: CreateDedicatedIpPool) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dedicated IP pool.
createDedicatedIpPool_poolName :: Lens.Lens' CreateDedicatedIpPool Prelude.Text
createDedicatedIpPool_poolName = Lens.lens (\CreateDedicatedIpPool' {poolName} -> poolName) (\s@CreateDedicatedIpPool' {} a -> s {poolName = a} :: CreateDedicatedIpPool)

instance Core.AWSRequest CreateDedicatedIpPool where
  type
    AWSResponse CreateDedicatedIpPool =
      CreateDedicatedIpPoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDedicatedIpPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDedicatedIpPool where
  hashWithSalt _salt CreateDedicatedIpPool' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` poolName

instance Prelude.NFData CreateDedicatedIpPool where
  rnf CreateDedicatedIpPool' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf poolName

instance Data.ToHeaders CreateDedicatedIpPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDedicatedIpPool where
  toJSON CreateDedicatedIpPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("PoolName" Data..= poolName)
          ]
      )

instance Data.ToPath CreateDedicatedIpPool where
  toPath = Prelude.const "/v1/email/dedicated-ip-pools"

instance Data.ToQuery CreateDedicatedIpPool where
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

instance Prelude.NFData CreateDedicatedIpPoolResponse where
  rnf CreateDedicatedIpPoolResponse' {..} =
    Prelude.rnf httpStatus
