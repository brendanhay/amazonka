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
-- Module      : Network.AWS.DirectConnect.StopBgpFailoverTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the virtual interface failover test.
module Network.AWS.DirectConnect.StopBgpFailoverTest
  ( -- * Creating a Request
    StopBgpFailoverTest (..),
    newStopBgpFailoverTest,

    -- * Request Lenses
    stopBgpFailoverTest_virtualInterfaceId,

    -- * Destructuring the Response
    StopBgpFailoverTestResponse (..),
    newStopBgpFailoverTestResponse,

    -- * Response Lenses
    stopBgpFailoverTestResponse_virtualInterfaceTest,
    stopBgpFailoverTestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopBgpFailoverTest' smart constructor.
data StopBgpFailoverTest = StopBgpFailoverTest'
  { -- | The ID of the virtual interface you no longer want to test.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBgpFailoverTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'stopBgpFailoverTest_virtualInterfaceId' - The ID of the virtual interface you no longer want to test.
newStopBgpFailoverTest ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  StopBgpFailoverTest
newStopBgpFailoverTest pVirtualInterfaceId_ =
  StopBgpFailoverTest'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface you no longer want to test.
stopBgpFailoverTest_virtualInterfaceId :: Lens.Lens' StopBgpFailoverTest Prelude.Text
stopBgpFailoverTest_virtualInterfaceId = Lens.lens (\StopBgpFailoverTest' {virtualInterfaceId} -> virtualInterfaceId) (\s@StopBgpFailoverTest' {} a -> s {virtualInterfaceId = a} :: StopBgpFailoverTest)

instance Core.AWSRequest StopBgpFailoverTest where
  type
    AWSResponse StopBgpFailoverTest =
      StopBgpFailoverTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopBgpFailoverTestResponse'
            Prelude.<$> (x Core..?> "virtualInterfaceTest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopBgpFailoverTest

instance Prelude.NFData StopBgpFailoverTest

instance Core.ToHeaders StopBgpFailoverTest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.StopBgpFailoverTest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopBgpFailoverTest where
  toJSON StopBgpFailoverTest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId)
          ]
      )

instance Core.ToPath StopBgpFailoverTest where
  toPath = Prelude.const "/"

instance Core.ToQuery StopBgpFailoverTest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBgpFailoverTestResponse' smart constructor.
data StopBgpFailoverTestResponse = StopBgpFailoverTestResponse'
  { -- | Information about the virtual interface failover test.
    virtualInterfaceTest :: Prelude.Maybe VirtualInterfaceTestHistory,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBgpFailoverTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceTest', 'stopBgpFailoverTestResponse_virtualInterfaceTest' - Information about the virtual interface failover test.
--
-- 'httpStatus', 'stopBgpFailoverTestResponse_httpStatus' - The response's http status code.
newStopBgpFailoverTestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopBgpFailoverTestResponse
newStopBgpFailoverTestResponse pHttpStatus_ =
  StopBgpFailoverTestResponse'
    { virtualInterfaceTest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the virtual interface failover test.
stopBgpFailoverTestResponse_virtualInterfaceTest :: Lens.Lens' StopBgpFailoverTestResponse (Prelude.Maybe VirtualInterfaceTestHistory)
stopBgpFailoverTestResponse_virtualInterfaceTest = Lens.lens (\StopBgpFailoverTestResponse' {virtualInterfaceTest} -> virtualInterfaceTest) (\s@StopBgpFailoverTestResponse' {} a -> s {virtualInterfaceTest = a} :: StopBgpFailoverTestResponse)

-- | The response's http status code.
stopBgpFailoverTestResponse_httpStatus :: Lens.Lens' StopBgpFailoverTestResponse Prelude.Int
stopBgpFailoverTestResponse_httpStatus = Lens.lens (\StopBgpFailoverTestResponse' {httpStatus} -> httpStatus) (\s@StopBgpFailoverTestResponse' {} a -> s {httpStatus = a} :: StopBgpFailoverTestResponse)

instance Prelude.NFData StopBgpFailoverTestResponse
