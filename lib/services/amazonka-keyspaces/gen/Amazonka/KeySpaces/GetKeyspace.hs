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
-- Module      : Amazonka.KeySpaces.GetKeyspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name and the Amazon Resource Name (ARN) of the specified
-- table.
module Amazonka.KeySpaces.GetKeyspace
  ( -- * Creating a Request
    GetKeyspace (..),
    newGetKeyspace,

    -- * Request Lenses
    getKeyspace_keyspaceName,

    -- * Destructuring the Response
    GetKeyspaceResponse (..),
    newGetKeyspaceResponse,

    -- * Response Lenses
    getKeyspaceResponse_httpStatus,
    getKeyspaceResponse_keyspaceName,
    getKeyspaceResponse_resourceArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyspace' smart constructor.
data GetKeyspace = GetKeyspace'
  { -- | The name of the keyspace.
    keyspaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyspaceName', 'getKeyspace_keyspaceName' - The name of the keyspace.
newGetKeyspace ::
  -- | 'keyspaceName'
  Prelude.Text ->
  GetKeyspace
newGetKeyspace pKeyspaceName_ =
  GetKeyspace' {keyspaceName = pKeyspaceName_}

-- | The name of the keyspace.
getKeyspace_keyspaceName :: Lens.Lens' GetKeyspace Prelude.Text
getKeyspace_keyspaceName = Lens.lens (\GetKeyspace' {keyspaceName} -> keyspaceName) (\s@GetKeyspace' {} a -> s {keyspaceName = a} :: GetKeyspace)

instance Core.AWSRequest GetKeyspace where
  type AWSResponse GetKeyspace = GetKeyspaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "keyspaceName")
            Prelude.<*> (x Data..:> "resourceArn")
      )

instance Prelude.Hashable GetKeyspace where
  hashWithSalt _salt GetKeyspace' {..} =
    _salt `Prelude.hashWithSalt` keyspaceName

instance Prelude.NFData GetKeyspace where
  rnf GetKeyspace' {..} = Prelude.rnf keyspaceName

instance Data.ToHeaders GetKeyspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.GetKeyspace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKeyspace where
  toJSON GetKeyspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("keyspaceName" Data..= keyspaceName)]
      )

instance Data.ToPath GetKeyspace where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKeyspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyspaceResponse' smart constructor.
data GetKeyspaceResponse = GetKeyspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the keyspace.
    keyspaceName :: Prelude.Text,
    -- | The ARN of the keyspace.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getKeyspaceResponse_httpStatus' - The response's http status code.
--
-- 'keyspaceName', 'getKeyspaceResponse_keyspaceName' - The name of the keyspace.
--
-- 'resourceArn', 'getKeyspaceResponse_resourceArn' - The ARN of the keyspace.
newGetKeyspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  GetKeyspaceResponse
newGetKeyspaceResponse
  pHttpStatus_
  pKeyspaceName_
  pResourceArn_ =
    GetKeyspaceResponse'
      { httpStatus = pHttpStatus_,
        keyspaceName = pKeyspaceName_,
        resourceArn = pResourceArn_
      }

-- | The response's http status code.
getKeyspaceResponse_httpStatus :: Lens.Lens' GetKeyspaceResponse Prelude.Int
getKeyspaceResponse_httpStatus = Lens.lens (\GetKeyspaceResponse' {httpStatus} -> httpStatus) (\s@GetKeyspaceResponse' {} a -> s {httpStatus = a} :: GetKeyspaceResponse)

-- | The name of the keyspace.
getKeyspaceResponse_keyspaceName :: Lens.Lens' GetKeyspaceResponse Prelude.Text
getKeyspaceResponse_keyspaceName = Lens.lens (\GetKeyspaceResponse' {keyspaceName} -> keyspaceName) (\s@GetKeyspaceResponse' {} a -> s {keyspaceName = a} :: GetKeyspaceResponse)

-- | The ARN of the keyspace.
getKeyspaceResponse_resourceArn :: Lens.Lens' GetKeyspaceResponse Prelude.Text
getKeyspaceResponse_resourceArn = Lens.lens (\GetKeyspaceResponse' {resourceArn} -> resourceArn) (\s@GetKeyspaceResponse' {} a -> s {resourceArn = a} :: GetKeyspaceResponse)

instance Prelude.NFData GetKeyspaceResponse where
  rnf GetKeyspaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf resourceArn
