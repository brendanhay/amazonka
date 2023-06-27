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
-- Module      : Amazonka.MediaConnect.DescribeBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of a bridge.
module Amazonka.MediaConnect.DescribeBridge
  ( -- * Creating a Request
    DescribeBridge (..),
    newDescribeBridge,

    -- * Request Lenses
    describeBridge_bridgeArn,

    -- * Destructuring the Response
    DescribeBridgeResponse (..),
    newDescribeBridgeResponse,

    -- * Response Lenses
    describeBridgeResponse_bridge,
    describeBridgeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBridge' smart constructor.
data DescribeBridge = DescribeBridge'
  { -- | The ARN of the bridge that you want to describe.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'describeBridge_bridgeArn' - The ARN of the bridge that you want to describe.
newDescribeBridge ::
  -- | 'bridgeArn'
  Prelude.Text ->
  DescribeBridge
newDescribeBridge pBridgeArn_ =
  DescribeBridge' {bridgeArn = pBridgeArn_}

-- | The ARN of the bridge that you want to describe.
describeBridge_bridgeArn :: Lens.Lens' DescribeBridge Prelude.Text
describeBridge_bridgeArn = Lens.lens (\DescribeBridge' {bridgeArn} -> bridgeArn) (\s@DescribeBridge' {} a -> s {bridgeArn = a} :: DescribeBridge)

instance Core.AWSRequest DescribeBridge where
  type
    AWSResponse DescribeBridge =
      DescribeBridgeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBridgeResponse'
            Prelude.<$> (x Data..?> "bridge")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBridge where
  hashWithSalt _salt DescribeBridge' {..} =
    _salt `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData DescribeBridge where
  rnf DescribeBridge' {..} = Prelude.rnf bridgeArn

instance Data.ToHeaders DescribeBridge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeBridge where
  toPath DescribeBridge' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn]

instance Data.ToQuery DescribeBridge where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBridgeResponse' smart constructor.
data DescribeBridgeResponse = DescribeBridgeResponse'
  { bridge :: Prelude.Maybe Bridge,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBridgeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridge', 'describeBridgeResponse_bridge' - Undocumented member.
--
-- 'httpStatus', 'describeBridgeResponse_httpStatus' - The response's http status code.
newDescribeBridgeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBridgeResponse
newDescribeBridgeResponse pHttpStatus_ =
  DescribeBridgeResponse'
    { bridge = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeBridgeResponse_bridge :: Lens.Lens' DescribeBridgeResponse (Prelude.Maybe Bridge)
describeBridgeResponse_bridge = Lens.lens (\DescribeBridgeResponse' {bridge} -> bridge) (\s@DescribeBridgeResponse' {} a -> s {bridge = a} :: DescribeBridgeResponse)

-- | The response's http status code.
describeBridgeResponse_httpStatus :: Lens.Lens' DescribeBridgeResponse Prelude.Int
describeBridgeResponse_httpStatus = Lens.lens (\DescribeBridgeResponse' {httpStatus} -> httpStatus) (\s@DescribeBridgeResponse' {} a -> s {httpStatus = a} :: DescribeBridgeResponse)

instance Prelude.NFData DescribeBridgeResponse where
  rnf DescribeBridgeResponse' {..} =
    Prelude.rnf bridge
      `Prelude.seq` Prelude.rnf httpStatus
