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
-- Module      : Amazonka.RedshiftServerLess.GetNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a namespace in Amazon Redshift Serverless.
module Amazonka.RedshiftServerLess.GetNamespace
  ( -- * Creating a Request
    GetNamespace (..),
    newGetNamespace,

    -- * Request Lenses
    getNamespace_namespaceName,

    -- * Destructuring the Response
    GetNamespaceResponse (..),
    newGetNamespaceResponse,

    -- * Response Lenses
    getNamespaceResponse_httpStatus,
    getNamespaceResponse_namespace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNamespace' smart constructor.
data GetNamespace = GetNamespace'
  { -- | The name of the namespace to retrieve information for.
    namespaceName :: Prelude.Text
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
-- 'namespaceName', 'getNamespace_namespaceName' - The name of the namespace to retrieve information for.
newGetNamespace ::
  -- | 'namespaceName'
  Prelude.Text ->
  GetNamespace
newGetNamespace pNamespaceName_ =
  GetNamespace' {namespaceName = pNamespaceName_}

-- | The name of the namespace to retrieve information for.
getNamespace_namespaceName :: Lens.Lens' GetNamespace Prelude.Text
getNamespace_namespaceName = Lens.lens (\GetNamespace' {namespaceName} -> namespaceName) (\s@GetNamespace' {} a -> s {namespaceName = a} :: GetNamespace)

instance Core.AWSRequest GetNamespace where
  type AWSResponse GetNamespace = GetNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamespaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "namespace")
      )

instance Prelude.Hashable GetNamespace where
  hashWithSalt _salt GetNamespace' {..} =
    _salt `Prelude.hashWithSalt` namespaceName

instance Prelude.NFData GetNamespace where
  rnf GetNamespace' {..} = Prelude.rnf namespaceName

instance Data.ToHeaders GetNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNamespace where
  toJSON GetNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("namespaceName" Data..= namespaceName)
          ]
      )

instance Data.ToPath GetNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery GetNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNamespaceResponse' smart constructor.
data GetNamespaceResponse = GetNamespaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The returned namespace object.
    namespace :: Namespace
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getNamespaceResponse_httpStatus' - The response's http status code.
--
-- 'namespace', 'getNamespaceResponse_namespace' - The returned namespace object.
newGetNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'namespace'
  Namespace ->
  GetNamespaceResponse
newGetNamespaceResponse pHttpStatus_ pNamespace_ =
  GetNamespaceResponse'
    { httpStatus = pHttpStatus_,
      namespace = pNamespace_
    }

-- | The response's http status code.
getNamespaceResponse_httpStatus :: Lens.Lens' GetNamespaceResponse Prelude.Int
getNamespaceResponse_httpStatus = Lens.lens (\GetNamespaceResponse' {httpStatus} -> httpStatus) (\s@GetNamespaceResponse' {} a -> s {httpStatus = a} :: GetNamespaceResponse)

-- | The returned namespace object.
getNamespaceResponse_namespace :: Lens.Lens' GetNamespaceResponse Namespace
getNamespaceResponse_namespace = Lens.lens (\GetNamespaceResponse' {namespace} -> namespace) (\s@GetNamespaceResponse' {} a -> s {namespace = a} :: GetNamespaceResponse)

instance Prelude.NFData GetNamespaceResponse where
  rnf GetNamespaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespace
