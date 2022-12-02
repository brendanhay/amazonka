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
-- Module      : Amazonka.CodeStarConnections.DeleteHost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The host to be deleted. Before you delete a host, all connections
-- associated to the host must be deleted.
--
-- A host cannot be deleted if it is in the VPC_CONFIG_INITIALIZING or
-- VPC_CONFIG_DELETING state.
module Amazonka.CodeStarConnections.DeleteHost
  ( -- * Creating a Request
    DeleteHost (..),
    newDeleteHost,

    -- * Request Lenses
    deleteHost_hostArn,

    -- * Destructuring the Response
    DeleteHostResponse (..),
    newDeleteHostResponse,

    -- * Response Lenses
    deleteHostResponse_httpStatus,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHost' smart constructor.
data DeleteHost = DeleteHost'
  { -- | The Amazon Resource Name (ARN) of the host to be deleted.
    hostArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostArn', 'deleteHost_hostArn' - The Amazon Resource Name (ARN) of the host to be deleted.
newDeleteHost ::
  -- | 'hostArn'
  Prelude.Text ->
  DeleteHost
newDeleteHost pHostArn_ =
  DeleteHost' {hostArn = pHostArn_}

-- | The Amazon Resource Name (ARN) of the host to be deleted.
deleteHost_hostArn :: Lens.Lens' DeleteHost Prelude.Text
deleteHost_hostArn = Lens.lens (\DeleteHost' {hostArn} -> hostArn) (\s@DeleteHost' {} a -> s {hostArn = a} :: DeleteHost)

instance Core.AWSRequest DeleteHost where
  type AWSResponse DeleteHost = DeleteHostResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHostResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHost where
  hashWithSalt _salt DeleteHost' {..} =
    _salt `Prelude.hashWithSalt` hostArn

instance Prelude.NFData DeleteHost where
  rnf DeleteHost' {..} = Prelude.rnf hostArn

instance Data.ToHeaders DeleteHost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.DeleteHost" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHost where
  toJSON DeleteHost' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HostArn" Data..= hostArn)]
      )

instance Data.ToPath DeleteHost where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHostResponse' smart constructor.
data DeleteHostResponse = DeleteHostResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHostResponse_httpStatus' - The response's http status code.
newDeleteHostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteHostResponse
newDeleteHostResponse pHttpStatus_ =
  DeleteHostResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteHostResponse_httpStatus :: Lens.Lens' DeleteHostResponse Prelude.Int
deleteHostResponse_httpStatus = Lens.lens (\DeleteHostResponse' {httpStatus} -> httpStatus) (\s@DeleteHostResponse' {} a -> s {httpStatus = a} :: DeleteHostResponse)

instance Prelude.NFData DeleteHostResponse where
  rnf DeleteHostResponse' {..} = Prelude.rnf httpStatus
