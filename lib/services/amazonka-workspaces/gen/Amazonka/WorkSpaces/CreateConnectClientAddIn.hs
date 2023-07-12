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
-- Module      : Amazonka.WorkSpaces.CreateConnectClientAddIn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a client-add-in for Amazon Connect within a directory. You can
-- create only one Amazon Connect client add-in within a directory.
--
-- This client add-in allows WorkSpaces users to seamlessly connect to
-- Amazon Connect.
module Amazonka.WorkSpaces.CreateConnectClientAddIn
  ( -- * Creating a Request
    CreateConnectClientAddIn (..),
    newCreateConnectClientAddIn,

    -- * Request Lenses
    createConnectClientAddIn_resourceId,
    createConnectClientAddIn_name,
    createConnectClientAddIn_url,

    -- * Destructuring the Response
    CreateConnectClientAddInResponse (..),
    newCreateConnectClientAddInResponse,

    -- * Response Lenses
    createConnectClientAddInResponse_addInId,
    createConnectClientAddInResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateConnectClientAddIn' smart constructor.
data CreateConnectClientAddIn = CreateConnectClientAddIn'
  { -- | The directory identifier for which to configure the client add-in.
    resourceId :: Prelude.Text,
    -- | The name of the client add-in.
    name :: Prelude.Text,
    -- | The endpoint URL of the Amazon Connect client add-in.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectClientAddIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'createConnectClientAddIn_resourceId' - The directory identifier for which to configure the client add-in.
--
-- 'name', 'createConnectClientAddIn_name' - The name of the client add-in.
--
-- 'url', 'createConnectClientAddIn_url' - The endpoint URL of the Amazon Connect client add-in.
newCreateConnectClientAddIn ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  CreateConnectClientAddIn
newCreateConnectClientAddIn pResourceId_ pName_ pURL_ =
  CreateConnectClientAddIn'
    { resourceId =
        pResourceId_,
      name = pName_,
      url = pURL_
    }

-- | The directory identifier for which to configure the client add-in.
createConnectClientAddIn_resourceId :: Lens.Lens' CreateConnectClientAddIn Prelude.Text
createConnectClientAddIn_resourceId = Lens.lens (\CreateConnectClientAddIn' {resourceId} -> resourceId) (\s@CreateConnectClientAddIn' {} a -> s {resourceId = a} :: CreateConnectClientAddIn)

-- | The name of the client add-in.
createConnectClientAddIn_name :: Lens.Lens' CreateConnectClientAddIn Prelude.Text
createConnectClientAddIn_name = Lens.lens (\CreateConnectClientAddIn' {name} -> name) (\s@CreateConnectClientAddIn' {} a -> s {name = a} :: CreateConnectClientAddIn)

-- | The endpoint URL of the Amazon Connect client add-in.
createConnectClientAddIn_url :: Lens.Lens' CreateConnectClientAddIn Prelude.Text
createConnectClientAddIn_url = Lens.lens (\CreateConnectClientAddIn' {url} -> url) (\s@CreateConnectClientAddIn' {} a -> s {url = a} :: CreateConnectClientAddIn)

instance Core.AWSRequest CreateConnectClientAddIn where
  type
    AWSResponse CreateConnectClientAddIn =
      CreateConnectClientAddInResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectClientAddInResponse'
            Prelude.<$> (x Data..?> "AddInId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectClientAddIn where
  hashWithSalt _salt CreateConnectClientAddIn' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData CreateConnectClientAddIn where
  rnf CreateConnectClientAddIn' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf url

instance Data.ToHeaders CreateConnectClientAddIn where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateConnectClientAddIn" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnectClientAddIn where
  toJSON CreateConnectClientAddIn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("URL" Data..= url)
          ]
      )

instance Data.ToPath CreateConnectClientAddIn where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnectClientAddIn where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectClientAddInResponse' smart constructor.
data CreateConnectClientAddInResponse = CreateConnectClientAddInResponse'
  { -- | The client add-in identifier.
    addInId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectClientAddInResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addInId', 'createConnectClientAddInResponse_addInId' - The client add-in identifier.
--
-- 'httpStatus', 'createConnectClientAddInResponse_httpStatus' - The response's http status code.
newCreateConnectClientAddInResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectClientAddInResponse
newCreateConnectClientAddInResponse pHttpStatus_ =
  CreateConnectClientAddInResponse'
    { addInId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The client add-in identifier.
createConnectClientAddInResponse_addInId :: Lens.Lens' CreateConnectClientAddInResponse (Prelude.Maybe Prelude.Text)
createConnectClientAddInResponse_addInId = Lens.lens (\CreateConnectClientAddInResponse' {addInId} -> addInId) (\s@CreateConnectClientAddInResponse' {} a -> s {addInId = a} :: CreateConnectClientAddInResponse)

-- | The response's http status code.
createConnectClientAddInResponse_httpStatus :: Lens.Lens' CreateConnectClientAddInResponse Prelude.Int
createConnectClientAddInResponse_httpStatus = Lens.lens (\CreateConnectClientAddInResponse' {httpStatus} -> httpStatus) (\s@CreateConnectClientAddInResponse' {} a -> s {httpStatus = a} :: CreateConnectClientAddInResponse)

instance
  Prelude.NFData
    CreateConnectClientAddInResponse
  where
  rnf CreateConnectClientAddInResponse' {..} =
    Prelude.rnf addInId
      `Prelude.seq` Prelude.rnf httpStatus
