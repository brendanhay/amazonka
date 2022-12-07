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
-- Module      : Amazonka.Transfer.DescribeHostKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the host key that\'s specified by the @HostKeyId@
-- and @ServerId@.
module Amazonka.Transfer.DescribeHostKey
  ( -- * Creating a Request
    DescribeHostKey (..),
    newDescribeHostKey,

    -- * Request Lenses
    describeHostKey_serverId,
    describeHostKey_hostKeyId,

    -- * Destructuring the Response
    DescribeHostKeyResponse (..),
    newDescribeHostKeyResponse,

    -- * Response Lenses
    describeHostKeyResponse_httpStatus,
    describeHostKeyResponse_hostKey,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeHostKey' smart constructor.
data DescribeHostKey = DescribeHostKey'
  { -- | The identifier of the server that contains the host key that you want
    -- described.
    serverId :: Prelude.Text,
    -- | The identifier of the host key that you want described.
    hostKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHostKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'describeHostKey_serverId' - The identifier of the server that contains the host key that you want
-- described.
--
-- 'hostKeyId', 'describeHostKey_hostKeyId' - The identifier of the host key that you want described.
newDescribeHostKey ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'hostKeyId'
  Prelude.Text ->
  DescribeHostKey
newDescribeHostKey pServerId_ pHostKeyId_ =
  DescribeHostKey'
    { serverId = pServerId_,
      hostKeyId = pHostKeyId_
    }

-- | The identifier of the server that contains the host key that you want
-- described.
describeHostKey_serverId :: Lens.Lens' DescribeHostKey Prelude.Text
describeHostKey_serverId = Lens.lens (\DescribeHostKey' {serverId} -> serverId) (\s@DescribeHostKey' {} a -> s {serverId = a} :: DescribeHostKey)

-- | The identifier of the host key that you want described.
describeHostKey_hostKeyId :: Lens.Lens' DescribeHostKey Prelude.Text
describeHostKey_hostKeyId = Lens.lens (\DescribeHostKey' {hostKeyId} -> hostKeyId) (\s@DescribeHostKey' {} a -> s {hostKeyId = a} :: DescribeHostKey)

instance Core.AWSRequest DescribeHostKey where
  type
    AWSResponse DescribeHostKey =
      DescribeHostKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHostKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HostKey")
      )

instance Prelude.Hashable DescribeHostKey where
  hashWithSalt _salt DescribeHostKey' {..} =
    _salt `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` hostKeyId

instance Prelude.NFData DescribeHostKey where
  rnf DescribeHostKey' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf hostKeyId

instance Data.ToHeaders DescribeHostKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DescribeHostKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHostKey where
  toJSON DescribeHostKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("HostKeyId" Data..= hostKeyId)
          ]
      )

instance Data.ToPath DescribeHostKey where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHostKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHostKeyResponse' smart constructor.
data DescribeHostKeyResponse = DescribeHostKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the details for the specified host key.
    hostKey :: DescribedHostKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHostKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeHostKeyResponse_httpStatus' - The response's http status code.
--
-- 'hostKey', 'describeHostKeyResponse_hostKey' - Returns the details for the specified host key.
newDescribeHostKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostKey'
  DescribedHostKey ->
  DescribeHostKeyResponse
newDescribeHostKeyResponse pHttpStatus_ pHostKey_ =
  DescribeHostKeyResponse'
    { httpStatus = pHttpStatus_,
      hostKey = pHostKey_
    }

-- | The response's http status code.
describeHostKeyResponse_httpStatus :: Lens.Lens' DescribeHostKeyResponse Prelude.Int
describeHostKeyResponse_httpStatus = Lens.lens (\DescribeHostKeyResponse' {httpStatus} -> httpStatus) (\s@DescribeHostKeyResponse' {} a -> s {httpStatus = a} :: DescribeHostKeyResponse)

-- | Returns the details for the specified host key.
describeHostKeyResponse_hostKey :: Lens.Lens' DescribeHostKeyResponse DescribedHostKey
describeHostKeyResponse_hostKey = Lens.lens (\DescribeHostKeyResponse' {hostKey} -> hostKey) (\s@DescribeHostKeyResponse' {} a -> s {hostKey = a} :: DescribeHostKeyResponse)

instance Prelude.NFData DescribeHostKeyResponse where
  rnf DescribeHostKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostKey
