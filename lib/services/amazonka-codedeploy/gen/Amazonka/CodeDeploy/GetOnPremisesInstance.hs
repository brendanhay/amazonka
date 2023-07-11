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
-- Module      : Amazonka.CodeDeploy.GetOnPremisesInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an on-premises instance.
module Amazonka.CodeDeploy.GetOnPremisesInstance
  ( -- * Creating a Request
    GetOnPremisesInstance (..),
    newGetOnPremisesInstance,

    -- * Request Lenses
    getOnPremisesInstance_instanceName,

    -- * Destructuring the Response
    GetOnPremisesInstanceResponse (..),
    newGetOnPremisesInstanceResponse,

    -- * Response Lenses
    getOnPremisesInstanceResponse_instanceInfo,
    getOnPremisesInstanceResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'newGetOnPremisesInstance' smart constructor.
data GetOnPremisesInstance = GetOnPremisesInstance'
  { -- | The name of the on-premises instance about which to get information.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOnPremisesInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getOnPremisesInstance_instanceName' - The name of the on-premises instance about which to get information.
newGetOnPremisesInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  GetOnPremisesInstance
newGetOnPremisesInstance pInstanceName_ =
  GetOnPremisesInstance'
    { instanceName =
        pInstanceName_
    }

-- | The name of the on-premises instance about which to get information.
getOnPremisesInstance_instanceName :: Lens.Lens' GetOnPremisesInstance Prelude.Text
getOnPremisesInstance_instanceName = Lens.lens (\GetOnPremisesInstance' {instanceName} -> instanceName) (\s@GetOnPremisesInstance' {} a -> s {instanceName = a} :: GetOnPremisesInstance)

instance Core.AWSRequest GetOnPremisesInstance where
  type
    AWSResponse GetOnPremisesInstance =
      GetOnPremisesInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOnPremisesInstanceResponse'
            Prelude.<$> (x Data..?> "instanceInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOnPremisesInstance where
  hashWithSalt _salt GetOnPremisesInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceName

instance Prelude.NFData GetOnPremisesInstance where
  rnf GetOnPremisesInstance' {..} =
    Prelude.rnf instanceName

instance Data.ToHeaders GetOnPremisesInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.GetOnPremisesInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOnPremisesInstance where
  toJSON GetOnPremisesInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceName" Data..= instanceName)]
      )

instance Data.ToPath GetOnPremisesInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOnPremisesInstance where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'newGetOnPremisesInstanceResponse' smart constructor.
data GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'
  { -- | Information about the on-premises instance.
    instanceInfo :: Prelude.Maybe InstanceInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOnPremisesInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInfo', 'getOnPremisesInstanceResponse_instanceInfo' - Information about the on-premises instance.
--
-- 'httpStatus', 'getOnPremisesInstanceResponse_httpStatus' - The response's http status code.
newGetOnPremisesInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOnPremisesInstanceResponse
newGetOnPremisesInstanceResponse pHttpStatus_ =
  GetOnPremisesInstanceResponse'
    { instanceInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the on-premises instance.
getOnPremisesInstanceResponse_instanceInfo :: Lens.Lens' GetOnPremisesInstanceResponse (Prelude.Maybe InstanceInfo)
getOnPremisesInstanceResponse_instanceInfo = Lens.lens (\GetOnPremisesInstanceResponse' {instanceInfo} -> instanceInfo) (\s@GetOnPremisesInstanceResponse' {} a -> s {instanceInfo = a} :: GetOnPremisesInstanceResponse)

-- | The response's http status code.
getOnPremisesInstanceResponse_httpStatus :: Lens.Lens' GetOnPremisesInstanceResponse Prelude.Int
getOnPremisesInstanceResponse_httpStatus = Lens.lens (\GetOnPremisesInstanceResponse' {httpStatus} -> httpStatus) (\s@GetOnPremisesInstanceResponse' {} a -> s {httpStatus = a} :: GetOnPremisesInstanceResponse)

instance Prelude.NFData GetOnPremisesInstanceResponse where
  rnf GetOnPremisesInstanceResponse' {..} =
    Prelude.rnf instanceInfo
      `Prelude.seq` Prelude.rnf httpStatus
