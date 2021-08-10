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
-- Module      : Network.AWS.Connect.DescribeQuickConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the quick connect.
module Network.AWS.Connect.DescribeQuickConnect
  ( -- * Creating a Request
    DescribeQuickConnect (..),
    newDescribeQuickConnect,

    -- * Request Lenses
    describeQuickConnect_instanceId,
    describeQuickConnect_quickConnectId,

    -- * Destructuring the Response
    DescribeQuickConnectResponse (..),
    newDescribeQuickConnectResponse,

    -- * Response Lenses
    describeQuickConnectResponse_quickConnect,
    describeQuickConnectResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQuickConnect' smart constructor.
data DescribeQuickConnect = DescribeQuickConnect'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeQuickConnect_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'quickConnectId', 'describeQuickConnect_quickConnectId' - The identifier for the quick connect.
newDescribeQuickConnect ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'quickConnectId'
  Prelude.Text ->
  DescribeQuickConnect
newDescribeQuickConnect pInstanceId_ pQuickConnectId_ =
  DescribeQuickConnect'
    { instanceId = pInstanceId_,
      quickConnectId = pQuickConnectId_
    }

-- | The identifier of the Amazon Connect instance.
describeQuickConnect_instanceId :: Lens.Lens' DescribeQuickConnect Prelude.Text
describeQuickConnect_instanceId = Lens.lens (\DescribeQuickConnect' {instanceId} -> instanceId) (\s@DescribeQuickConnect' {} a -> s {instanceId = a} :: DescribeQuickConnect)

-- | The identifier for the quick connect.
describeQuickConnect_quickConnectId :: Lens.Lens' DescribeQuickConnect Prelude.Text
describeQuickConnect_quickConnectId = Lens.lens (\DescribeQuickConnect' {quickConnectId} -> quickConnectId) (\s@DescribeQuickConnect' {} a -> s {quickConnectId = a} :: DescribeQuickConnect)

instance Core.AWSRequest DescribeQuickConnect where
  type
    AWSResponse DescribeQuickConnect =
      DescribeQuickConnectResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQuickConnectResponse'
            Prelude.<$> (x Core..?> "QuickConnect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeQuickConnect

instance Prelude.NFData DescribeQuickConnect

instance Core.ToHeaders DescribeQuickConnect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeQuickConnect where
  toPath DescribeQuickConnect' {..} =
    Prelude.mconcat
      [ "/quick-connects/",
        Core.toBS instanceId,
        "/",
        Core.toBS quickConnectId
      ]

instance Core.ToQuery DescribeQuickConnect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQuickConnectResponse' smart constructor.
data DescribeQuickConnectResponse = DescribeQuickConnectResponse'
  { -- | Information about the quick connect.
    quickConnect :: Prelude.Maybe QuickConnect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuickConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quickConnect', 'describeQuickConnectResponse_quickConnect' - Information about the quick connect.
--
-- 'httpStatus', 'describeQuickConnectResponse_httpStatus' - The response's http status code.
newDescribeQuickConnectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQuickConnectResponse
newDescribeQuickConnectResponse pHttpStatus_ =
  DescribeQuickConnectResponse'
    { quickConnect =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the quick connect.
describeQuickConnectResponse_quickConnect :: Lens.Lens' DescribeQuickConnectResponse (Prelude.Maybe QuickConnect)
describeQuickConnectResponse_quickConnect = Lens.lens (\DescribeQuickConnectResponse' {quickConnect} -> quickConnect) (\s@DescribeQuickConnectResponse' {} a -> s {quickConnect = a} :: DescribeQuickConnectResponse)

-- | The response's http status code.
describeQuickConnectResponse_httpStatus :: Lens.Lens' DescribeQuickConnectResponse Prelude.Int
describeQuickConnectResponse_httpStatus = Lens.lens (\DescribeQuickConnectResponse' {httpStatus} -> httpStatus) (\s@DescribeQuickConnectResponse' {} a -> s {httpStatus = a} :: DescribeQuickConnectResponse)

instance Prelude.NFData DescribeQuickConnectResponse
