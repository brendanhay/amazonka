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
-- Module      : Network.AWS.Route53AutoNaming.GetNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a namespace.
module Network.AWS.Route53AutoNaming.GetNamespace
  ( -- * Creating a Request
    GetNamespace (..),
    newGetNamespace,

    -- * Request Lenses
    getNamespace_id,

    -- * Destructuring the Response
    GetNamespaceResponse (..),
    newGetNamespaceResponse,

    -- * Response Lenses
    getNamespaceResponse_namespace,
    getNamespaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newGetNamespace' smart constructor.
data GetNamespace = GetNamespace'
  { -- | The ID of the namespace that you want to get information about.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getNamespace_id' - The ID of the namespace that you want to get information about.
newGetNamespace ::
  -- | 'id'
  Prelude.Text ->
  GetNamespace
newGetNamespace pId_ = GetNamespace' {id = pId_}

-- | The ID of the namespace that you want to get information about.
getNamespace_id :: Lens.Lens' GetNamespace Prelude.Text
getNamespace_id = Lens.lens (\GetNamespace' {id} -> id) (\s@GetNamespace' {} a -> s {id = a} :: GetNamespace)

instance Core.AWSRequest GetNamespace where
  type AWSResponse GetNamespace = GetNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamespaceResponse'
            Prelude.<$> (x Core..?> "Namespace")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNamespace

instance Prelude.NFData GetNamespace

instance Core.ToHeaders GetNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.GetNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetNamespace where
  toJSON GetNamespace' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("Id" Core..= id)])

instance Core.ToPath GetNamespace where
  toPath = Prelude.const "/"

instance Core.ToQuery GetNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNamespaceResponse' smart constructor.
data GetNamespaceResponse = GetNamespaceResponse'
  { -- | A complex type that contains information about the specified namespace.
    namespace :: Prelude.Maybe Namespace,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'getNamespaceResponse_namespace' - A complex type that contains information about the specified namespace.
--
-- 'httpStatus', 'getNamespaceResponse_httpStatus' - The response's http status code.
newGetNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNamespaceResponse
newGetNamespaceResponse pHttpStatus_ =
  GetNamespaceResponse'
    { namespace = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about the specified namespace.
getNamespaceResponse_namespace :: Lens.Lens' GetNamespaceResponse (Prelude.Maybe Namespace)
getNamespaceResponse_namespace = Lens.lens (\GetNamespaceResponse' {namespace} -> namespace) (\s@GetNamespaceResponse' {} a -> s {namespace = a} :: GetNamespaceResponse)

-- | The response's http status code.
getNamespaceResponse_httpStatus :: Lens.Lens' GetNamespaceResponse Prelude.Int
getNamespaceResponse_httpStatus = Lens.lens (\GetNamespaceResponse' {httpStatus} -> httpStatus) (\s@GetNamespaceResponse' {} a -> s {httpStatus = a} :: GetNamespaceResponse)

instance Prelude.NFData GetNamespaceResponse
