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
-- Module      : Network.AWS.DirectConnect.StartBgpFailoverTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the virtual interface failover test that verifies your
-- configuration meets your resiliency requirements by placing the BGP
-- peering session in the DOWN state. You can then send traffic to verify
-- that there are no outages.
--
-- You can run the test on public, private, transit, and hosted virtual
-- interfaces.
--
-- You can use
-- <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_ListVirtualInterfaceTestHistory.html ListVirtualInterfaceTestHistory>
-- to view the virtual interface test history.
--
-- If you need to stop the test before the test interval completes, use
-- <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_StopBgpFailoverTest.html StopBgpFailoverTest>.
module Network.AWS.DirectConnect.StartBgpFailoverTest
  ( -- * Creating a Request
    StartBgpFailoverTest (..),
    newStartBgpFailoverTest,

    -- * Request Lenses
    startBgpFailoverTest_bgpPeers,
    startBgpFailoverTest_testDurationInMinutes,
    startBgpFailoverTest_virtualInterfaceId,

    -- * Destructuring the Response
    StartBgpFailoverTestResponse (..),
    newStartBgpFailoverTestResponse,

    -- * Response Lenses
    startBgpFailoverTestResponse_virtualInterfaceTest,
    startBgpFailoverTestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartBgpFailoverTest' smart constructor.
data StartBgpFailoverTest = StartBgpFailoverTest'
  { -- | The BGP peers to place in the DOWN state.
    bgpPeers :: Prelude.Maybe [Prelude.Text],
    -- | The time in minutes that the virtual interface failover test will last.
    --
    -- Maximum value: 180 minutes (3 hours).
    --
    -- Default: 180 minutes (3 hours).
    testDurationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual interface you want to test.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBgpFailoverTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpPeers', 'startBgpFailoverTest_bgpPeers' - The BGP peers to place in the DOWN state.
--
-- 'testDurationInMinutes', 'startBgpFailoverTest_testDurationInMinutes' - The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
--
-- Default: 180 minutes (3 hours).
--
-- 'virtualInterfaceId', 'startBgpFailoverTest_virtualInterfaceId' - The ID of the virtual interface you want to test.
newStartBgpFailoverTest ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  StartBgpFailoverTest
newStartBgpFailoverTest pVirtualInterfaceId_ =
  StartBgpFailoverTest'
    { bgpPeers = Prelude.Nothing,
      testDurationInMinutes = Prelude.Nothing,
      virtualInterfaceId = pVirtualInterfaceId_
    }

-- | The BGP peers to place in the DOWN state.
startBgpFailoverTest_bgpPeers :: Lens.Lens' StartBgpFailoverTest (Prelude.Maybe [Prelude.Text])
startBgpFailoverTest_bgpPeers = Lens.lens (\StartBgpFailoverTest' {bgpPeers} -> bgpPeers) (\s@StartBgpFailoverTest' {} a -> s {bgpPeers = a} :: StartBgpFailoverTest) Prelude.. Lens.mapping Lens._Coerce

-- | The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
--
-- Default: 180 minutes (3 hours).
startBgpFailoverTest_testDurationInMinutes :: Lens.Lens' StartBgpFailoverTest (Prelude.Maybe Prelude.Int)
startBgpFailoverTest_testDurationInMinutes = Lens.lens (\StartBgpFailoverTest' {testDurationInMinutes} -> testDurationInMinutes) (\s@StartBgpFailoverTest' {} a -> s {testDurationInMinutes = a} :: StartBgpFailoverTest)

-- | The ID of the virtual interface you want to test.
startBgpFailoverTest_virtualInterfaceId :: Lens.Lens' StartBgpFailoverTest Prelude.Text
startBgpFailoverTest_virtualInterfaceId = Lens.lens (\StartBgpFailoverTest' {virtualInterfaceId} -> virtualInterfaceId) (\s@StartBgpFailoverTest' {} a -> s {virtualInterfaceId = a} :: StartBgpFailoverTest)

instance Core.AWSRequest StartBgpFailoverTest where
  type
    AWSResponse StartBgpFailoverTest =
      StartBgpFailoverTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBgpFailoverTestResponse'
            Prelude.<$> (x Core..?> "virtualInterfaceTest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBgpFailoverTest

instance Prelude.NFData StartBgpFailoverTest

instance Core.ToHeaders StartBgpFailoverTest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.StartBgpFailoverTest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartBgpFailoverTest where
  toJSON StartBgpFailoverTest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bgpPeers" Core..=) Prelude.<$> bgpPeers,
            ("testDurationInMinutes" Core..=)
              Prelude.<$> testDurationInMinutes,
            Prelude.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId)
          ]
      )

instance Core.ToPath StartBgpFailoverTest where
  toPath = Prelude.const "/"

instance Core.ToQuery StartBgpFailoverTest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBgpFailoverTestResponse' smart constructor.
data StartBgpFailoverTestResponse = StartBgpFailoverTestResponse'
  { -- | Information about the virtual interface failover test.
    virtualInterfaceTest :: Prelude.Maybe VirtualInterfaceTestHistory,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBgpFailoverTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceTest', 'startBgpFailoverTestResponse_virtualInterfaceTest' - Information about the virtual interface failover test.
--
-- 'httpStatus', 'startBgpFailoverTestResponse_httpStatus' - The response's http status code.
newStartBgpFailoverTestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBgpFailoverTestResponse
newStartBgpFailoverTestResponse pHttpStatus_ =
  StartBgpFailoverTestResponse'
    { virtualInterfaceTest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the virtual interface failover test.
startBgpFailoverTestResponse_virtualInterfaceTest :: Lens.Lens' StartBgpFailoverTestResponse (Prelude.Maybe VirtualInterfaceTestHistory)
startBgpFailoverTestResponse_virtualInterfaceTest = Lens.lens (\StartBgpFailoverTestResponse' {virtualInterfaceTest} -> virtualInterfaceTest) (\s@StartBgpFailoverTestResponse' {} a -> s {virtualInterfaceTest = a} :: StartBgpFailoverTestResponse)

-- | The response's http status code.
startBgpFailoverTestResponse_httpStatus :: Lens.Lens' StartBgpFailoverTestResponse Prelude.Int
startBgpFailoverTestResponse_httpStatus = Lens.lens (\StartBgpFailoverTestResponse' {httpStatus} -> httpStatus) (\s@StartBgpFailoverTestResponse' {} a -> s {httpStatus = a} :: StartBgpFailoverTestResponse)

instance Prelude.NFData StartBgpFailoverTestResponse
