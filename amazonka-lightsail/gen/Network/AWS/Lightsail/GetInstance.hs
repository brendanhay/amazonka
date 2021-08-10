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
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is
-- a virtual private server.
module Network.AWS.Lightsail.GetInstance
  ( -- * Creating a Request
    GetInstance (..),
    newGetInstance,

    -- * Request Lenses
    getInstance_instanceName,

    -- * Destructuring the Response
    GetInstanceResponse (..),
    newGetInstanceResponse,

    -- * Response Lenses
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstance' smart constructor.
data GetInstance = GetInstance'
  { -- | The name of the instance.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'getInstance_instanceName' - The name of the instance.
newGetInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  GetInstance
newGetInstance pInstanceName_ =
  GetInstance' {instanceName = pInstanceName_}

-- | The name of the instance.
getInstance_instanceName :: Lens.Lens' GetInstance Prelude.Text
getInstance_instanceName = Lens.lens (\GetInstance' {instanceName} -> instanceName) (\s@GetInstance' {} a -> s {instanceName = a} :: GetInstance)

instance Core.AWSRequest GetInstance where
  type AWSResponse GetInstance = GetInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Prelude.<$> (x Core..?> "instance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstance

instance Prelude.NFData GetInstance

instance Core.ToHeaders GetInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath GetInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { -- | An array of key-value pairs containing information about the specified
    -- instance.
    instance' :: Prelude.Maybe Instance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instance'', 'getInstanceResponse_instance' - An array of key-value pairs containing information about the specified
-- instance.
--
-- 'httpStatus', 'getInstanceResponse_httpStatus' - The response's http status code.
newGetInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceResponse
newGetInstanceResponse pHttpStatus_ =
  GetInstanceResponse'
    { instance' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the specified
-- instance.
getInstanceResponse_instance :: Lens.Lens' GetInstanceResponse (Prelude.Maybe Instance)
getInstanceResponse_instance = Lens.lens (\GetInstanceResponse' {instance'} -> instance') (\s@GetInstanceResponse' {} a -> s {instance' = a} :: GetInstanceResponse)

-- | The response's http status code.
getInstanceResponse_httpStatus :: Lens.Lens' GetInstanceResponse Prelude.Int
getInstanceResponse_httpStatus = Lens.lens (\GetInstanceResponse' {httpStatus} -> httpStatus) (\s@GetInstanceResponse' {} a -> s {httpStatus = a} :: GetInstanceResponse)

instance Prelude.NFData GetInstanceResponse
