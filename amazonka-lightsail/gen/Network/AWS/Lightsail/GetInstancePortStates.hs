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
-- Module      : Network.AWS.Lightsail.GetInstancePortStates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the firewall port states for a specific Amazon Lightsail
-- instance, the IP addresses allowed to connect to the instance through
-- the ports, and the protocol.
module Network.AWS.Lightsail.GetInstancePortStates
  ( -- * Creating a Request
    GetInstancePortStates (..),
    newGetInstancePortStates,

    -- * Request Lenses
    getInstancePortStates_instanceName,

    -- * Destructuring the Response
    GetInstancePortStatesResponse (..),
    newGetInstancePortStatesResponse,

    -- * Response Lenses
    getInstancePortStatesResponse_portStates,
    getInstancePortStatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstancePortStates' smart constructor.
data GetInstancePortStates = GetInstancePortStates'
  { -- | The name of the instance for which to return firewall port states.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstancePortStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getInstancePortStates_instanceName' - The name of the instance for which to return firewall port states.
newGetInstancePortStates ::
  -- | 'instanceName'
  Prelude.Text ->
  GetInstancePortStates
newGetInstancePortStates pInstanceName_ =
  GetInstancePortStates'
    { instanceName =
        pInstanceName_
    }

-- | The name of the instance for which to return firewall port states.
getInstancePortStates_instanceName :: Lens.Lens' GetInstancePortStates Prelude.Text
getInstancePortStates_instanceName = Lens.lens (\GetInstancePortStates' {instanceName} -> instanceName) (\s@GetInstancePortStates' {} a -> s {instanceName = a} :: GetInstancePortStates)

instance Core.AWSRequest GetInstancePortStates where
  type
    AWSResponse GetInstancePortStates =
      GetInstancePortStatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancePortStatesResponse'
            Prelude.<$> (x Core..?> "portStates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstancePortStates

instance Prelude.NFData GetInstancePortStates

instance Core.ToHeaders GetInstancePortStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstancePortStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInstancePortStates where
  toJSON GetInstancePortStates' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath GetInstancePortStates where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInstancePortStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstancePortStatesResponse' smart constructor.
data GetInstancePortStatesResponse = GetInstancePortStatesResponse'
  { -- | An array of objects that describe the firewall port states for the
    -- specified instance.
    portStates :: Prelude.Maybe [InstancePortState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstancePortStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portStates', 'getInstancePortStatesResponse_portStates' - An array of objects that describe the firewall port states for the
-- specified instance.
--
-- 'httpStatus', 'getInstancePortStatesResponse_httpStatus' - The response's http status code.
newGetInstancePortStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstancePortStatesResponse
newGetInstancePortStatesResponse pHttpStatus_ =
  GetInstancePortStatesResponse'
    { portStates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the firewall port states for the
-- specified instance.
getInstancePortStatesResponse_portStates :: Lens.Lens' GetInstancePortStatesResponse (Prelude.Maybe [InstancePortState])
getInstancePortStatesResponse_portStates = Lens.lens (\GetInstancePortStatesResponse' {portStates} -> portStates) (\s@GetInstancePortStatesResponse' {} a -> s {portStates = a} :: GetInstancePortStatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getInstancePortStatesResponse_httpStatus :: Lens.Lens' GetInstancePortStatesResponse Prelude.Int
getInstancePortStatesResponse_httpStatus = Lens.lens (\GetInstancePortStatesResponse' {httpStatus} -> httpStatus) (\s@GetInstancePortStatesResponse' {} a -> s {httpStatus = a} :: GetInstancePortStatesResponse)

instance Prelude.NFData GetInstancePortStatesResponse
