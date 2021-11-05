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
-- Module      : Amazonka.IoTThingsGraph.GetSystemInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a system instance.
module Amazonka.IoTThingsGraph.GetSystemInstance
  ( -- * Creating a Request
    GetSystemInstance (..),
    newGetSystemInstance,

    -- * Request Lenses
    getSystemInstance_id,

    -- * Destructuring the Response
    GetSystemInstanceResponse (..),
    newGetSystemInstanceResponse,

    -- * Response Lenses
    getSystemInstanceResponse_description,
    getSystemInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSystemInstance' smart constructor.
data GetSystemInstance = GetSystemInstance'
  { -- | The ID of the system deployment instance. This value is returned by
    -- @CreateSystemInstance@.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getSystemInstance_id' - The ID of the system deployment instance. This value is returned by
-- @CreateSystemInstance@.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
newGetSystemInstance ::
  -- | 'id'
  Prelude.Text ->
  GetSystemInstance
newGetSystemInstance pId_ =
  GetSystemInstance' {id = pId_}

-- | The ID of the system deployment instance. This value is returned by
-- @CreateSystemInstance@.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
getSystemInstance_id :: Lens.Lens' GetSystemInstance Prelude.Text
getSystemInstance_id = Lens.lens (\GetSystemInstance' {id} -> id) (\s@GetSystemInstance' {} a -> s {id = a} :: GetSystemInstance)

instance Core.AWSRequest GetSystemInstance where
  type
    AWSResponse GetSystemInstance =
      GetSystemInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSystemInstanceResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSystemInstance

instance Prelude.NFData GetSystemInstance

instance Core.ToHeaders GetSystemInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetSystemInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSystemInstance where
  toJSON GetSystemInstance' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath GetSystemInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSystemInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSystemInstanceResponse' smart constructor.
data GetSystemInstanceResponse = GetSystemInstanceResponse'
  { -- | An object that describes the system instance.
    description :: Prelude.Maybe SystemInstanceDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSystemInstanceResponse_description' - An object that describes the system instance.
--
-- 'httpStatus', 'getSystemInstanceResponse_httpStatus' - The response's http status code.
newGetSystemInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSystemInstanceResponse
newGetSystemInstanceResponse pHttpStatus_ =
  GetSystemInstanceResponse'
    { description =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the system instance.
getSystemInstanceResponse_description :: Lens.Lens' GetSystemInstanceResponse (Prelude.Maybe SystemInstanceDescription)
getSystemInstanceResponse_description = Lens.lens (\GetSystemInstanceResponse' {description} -> description) (\s@GetSystemInstanceResponse' {} a -> s {description = a} :: GetSystemInstanceResponse)

-- | The response's http status code.
getSystemInstanceResponse_httpStatus :: Lens.Lens' GetSystemInstanceResponse Prelude.Int
getSystemInstanceResponse_httpStatus = Lens.lens (\GetSystemInstanceResponse' {httpStatus} -> httpStatus) (\s@GetSystemInstanceResponse' {} a -> s {httpStatus = a} :: GetSystemInstanceResponse)

instance Prelude.NFData GetSystemInstanceResponse
