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
-- Module      : Amazonka.FMS.GetResourceSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific resource set.
module Amazonka.FMS.GetResourceSet
  ( -- * Creating a Request
    GetResourceSet (..),
    newGetResourceSet,

    -- * Request Lenses
    getResourceSet_identifier,

    -- * Destructuring the Response
    GetResourceSetResponse (..),
    newGetResourceSetResponse,

    -- * Response Lenses
    getResourceSetResponse_httpStatus,
    getResourceSetResponse_resourceSet,
    getResourceSetResponse_resourceSetArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceSet' smart constructor.
data GetResourceSet = GetResourceSet'
  { -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getResourceSet_identifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
newGetResourceSet ::
  -- | 'identifier'
  Prelude.Text ->
  GetResourceSet
newGetResourceSet pIdentifier_ =
  GetResourceSet' {identifier = pIdentifier_}

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
getResourceSet_identifier :: Lens.Lens' GetResourceSet Prelude.Text
getResourceSet_identifier = Lens.lens (\GetResourceSet' {identifier} -> identifier) (\s@GetResourceSet' {} a -> s {identifier = a} :: GetResourceSet)

instance Core.AWSRequest GetResourceSet where
  type
    AWSResponse GetResourceSet =
      GetResourceSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ResourceSet")
            Prelude.<*> (x Core..:> "ResourceSetArn")
      )

instance Prelude.Hashable GetResourceSet where
  hashWithSalt _salt GetResourceSet' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetResourceSet where
  rnf GetResourceSet' {..} = Prelude.rnf identifier

instance Core.ToHeaders GetResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetResourceSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourceSet where
  toJSON GetResourceSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Core..= identifier)]
      )

instance Core.ToPath GetResourceSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceSetResponse' smart constructor.
data GetResourceSetResponse = GetResourceSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the specified resource set.
    resourceSet :: ResourceSet,
    -- | The Amazon Resource Name (ARN) of the resource set.
    resourceSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getResourceSetResponse_httpStatus' - The response's http status code.
--
-- 'resourceSet', 'getResourceSetResponse_resourceSet' - Information about the specified resource set.
--
-- 'resourceSetArn', 'getResourceSetResponse_resourceSetArn' - The Amazon Resource Name (ARN) of the resource set.
newGetResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceSet'
  ResourceSet ->
  -- | 'resourceSetArn'
  Prelude.Text ->
  GetResourceSetResponse
newGetResourceSetResponse
  pHttpStatus_
  pResourceSet_
  pResourceSetArn_ =
    GetResourceSetResponse'
      { httpStatus = pHttpStatus_,
        resourceSet = pResourceSet_,
        resourceSetArn = pResourceSetArn_
      }

-- | The response's http status code.
getResourceSetResponse_httpStatus :: Lens.Lens' GetResourceSetResponse Prelude.Int
getResourceSetResponse_httpStatus = Lens.lens (\GetResourceSetResponse' {httpStatus} -> httpStatus) (\s@GetResourceSetResponse' {} a -> s {httpStatus = a} :: GetResourceSetResponse)

-- | Information about the specified resource set.
getResourceSetResponse_resourceSet :: Lens.Lens' GetResourceSetResponse ResourceSet
getResourceSetResponse_resourceSet = Lens.lens (\GetResourceSetResponse' {resourceSet} -> resourceSet) (\s@GetResourceSetResponse' {} a -> s {resourceSet = a} :: GetResourceSetResponse)

-- | The Amazon Resource Name (ARN) of the resource set.
getResourceSetResponse_resourceSetArn :: Lens.Lens' GetResourceSetResponse Prelude.Text
getResourceSetResponse_resourceSetArn = Lens.lens (\GetResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@GetResourceSetResponse' {} a -> s {resourceSetArn = a} :: GetResourceSetResponse)

instance Prelude.NFData GetResourceSetResponse where
  rnf GetResourceSetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceSet
      `Prelude.seq` Prelude.rnf resourceSetArn
