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
-- Module      : Amazonka.Transfer.DescribeServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a file transfer protocol-enabled server that you specify by
-- passing the @ServerId@ parameter.
--
-- The response contains a description of a server\'s properties. When you
-- set @EndpointType@ to VPC, the response will contain the
-- @EndpointDetails@.
module Amazonka.Transfer.DescribeServer
  ( -- * Creating a Request
    DescribeServer (..),
    newDescribeServer,

    -- * Request Lenses
    describeServer_serverId,

    -- * Destructuring the Response
    DescribeServerResponse (..),
    newDescribeServerResponse,

    -- * Response Lenses
    describeServerResponse_httpStatus,
    describeServerResponse_server,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeServer' smart constructor.
data DescribeServer = DescribeServer'
  { -- | A system-assigned unique identifier for a server.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'describeServer_serverId' - A system-assigned unique identifier for a server.
newDescribeServer ::
  -- | 'serverId'
  Prelude.Text ->
  DescribeServer
newDescribeServer pServerId_ =
  DescribeServer' {serverId = pServerId_}

-- | A system-assigned unique identifier for a server.
describeServer_serverId :: Lens.Lens' DescribeServer Prelude.Text
describeServer_serverId = Lens.lens (\DescribeServer' {serverId} -> serverId) (\s@DescribeServer' {} a -> s {serverId = a} :: DescribeServer)

instance Core.AWSRequest DescribeServer where
  type
    AWSResponse DescribeServer =
      DescribeServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Server")
      )

instance Prelude.Hashable DescribeServer where
  hashWithSalt _salt DescribeServer' {..} =
    _salt `Prelude.hashWithSalt` serverId

instance Prelude.NFData DescribeServer where
  rnf DescribeServer' {..} = Prelude.rnf serverId

instance Core.ToHeaders DescribeServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DescribeServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeServer where
  toJSON DescribeServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServerId" Core..= serverId)]
      )

instance Core.ToPath DescribeServer where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServerResponse' smart constructor.
data DescribeServerResponse = DescribeServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array containing the properties of a server with the @ServerID@ you
    -- specified.
    server :: DescribedServer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeServerResponse_httpStatus' - The response's http status code.
--
-- 'server', 'describeServerResponse_server' - An array containing the properties of a server with the @ServerID@ you
-- specified.
newDescribeServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'server'
  DescribedServer ->
  DescribeServerResponse
newDescribeServerResponse pHttpStatus_ pServer_ =
  DescribeServerResponse'
    { httpStatus = pHttpStatus_,
      server = pServer_
    }

-- | The response's http status code.
describeServerResponse_httpStatus :: Lens.Lens' DescribeServerResponse Prelude.Int
describeServerResponse_httpStatus = Lens.lens (\DescribeServerResponse' {httpStatus} -> httpStatus) (\s@DescribeServerResponse' {} a -> s {httpStatus = a} :: DescribeServerResponse)

-- | An array containing the properties of a server with the @ServerID@ you
-- specified.
describeServerResponse_server :: Lens.Lens' DescribeServerResponse DescribedServer
describeServerResponse_server = Lens.lens (\DescribeServerResponse' {server} -> server) (\s@DescribeServerResponse' {} a -> s {server = a} :: DescribeServerResponse)

instance Prelude.NFData DescribeServerResponse where
  rnf DescribeServerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf server
