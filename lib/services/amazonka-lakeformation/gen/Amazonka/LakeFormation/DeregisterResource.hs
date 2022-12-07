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
-- Module      : Amazonka.LakeFormation.DeregisterResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the resource as managed by the Data Catalog.
--
-- When you deregister a path, Lake Formation removes the path from the
-- inline policy attached to your service-linked role.
module Amazonka.LakeFormation.DeregisterResource
  ( -- * Creating a Request
    DeregisterResource (..),
    newDeregisterResource,

    -- * Request Lenses
    deregisterResource_resourceArn,

    -- * Destructuring the Response
    DeregisterResourceResponse (..),
    newDeregisterResourceResponse,

    -- * Response Lenses
    deregisterResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterResource' smart constructor.
data DeregisterResource = DeregisterResource'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to
    -- deregister.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deregisterResource_resourceArn' - The Amazon Resource Name (ARN) of the resource that you want to
-- deregister.
newDeregisterResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeregisterResource
newDeregisterResource pResourceArn_ =
  DeregisterResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the resource that you want to
-- deregister.
deregisterResource_resourceArn :: Lens.Lens' DeregisterResource Prelude.Text
deregisterResource_resourceArn = Lens.lens (\DeregisterResource' {resourceArn} -> resourceArn) (\s@DeregisterResource' {} a -> s {resourceArn = a} :: DeregisterResource)

instance Core.AWSRequest DeregisterResource where
  type
    AWSResponse DeregisterResource =
      DeregisterResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterResource where
  hashWithSalt _salt DeregisterResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeregisterResource where
  rnf DeregisterResource' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders DeregisterResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterResource where
  toJSON DeregisterResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DeregisterResource where
  toPath = Prelude.const "/DeregisterResource"

instance Data.ToQuery DeregisterResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterResourceResponse' smart constructor.
data DeregisterResourceResponse = DeregisterResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterResourceResponse_httpStatus' - The response's http status code.
newDeregisterResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterResourceResponse
newDeregisterResourceResponse pHttpStatus_ =
  DeregisterResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterResourceResponse_httpStatus :: Lens.Lens' DeregisterResourceResponse Prelude.Int
deregisterResourceResponse_httpStatus = Lens.lens (\DeregisterResourceResponse' {httpStatus} -> httpStatus) (\s@DeregisterResourceResponse' {} a -> s {httpStatus = a} :: DeregisterResourceResponse)

instance Prelude.NFData DeregisterResourceResponse where
  rnf DeregisterResourceResponse' {..} =
    Prelude.rnf httpStatus
