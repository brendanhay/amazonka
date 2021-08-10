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
-- Module      : Network.AWS.Lightsail.AttachStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a static IP address to a specific Amazon Lightsail instance.
module Network.AWS.Lightsail.AttachStaticIp
  ( -- * Creating a Request
    AttachStaticIp (..),
    newAttachStaticIp,

    -- * Request Lenses
    attachStaticIp_staticIpName,
    attachStaticIp_instanceName,

    -- * Destructuring the Response
    AttachStaticIpResponse (..),
    newAttachStaticIpResponse,

    -- * Response Lenses
    attachStaticIpResponse_operations,
    attachStaticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachStaticIp' smart constructor.
data AttachStaticIp = AttachStaticIp'
  { -- | The name of the static IP.
    staticIpName :: Prelude.Text,
    -- | The instance name to which you want to attach the static IP address.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'attachStaticIp_staticIpName' - The name of the static IP.
--
-- 'instanceName', 'attachStaticIp_instanceName' - The instance name to which you want to attach the static IP address.
newAttachStaticIp ::
  -- | 'staticIpName'
  Prelude.Text ->
  -- | 'instanceName'
  Prelude.Text ->
  AttachStaticIp
newAttachStaticIp pStaticIpName_ pInstanceName_ =
  AttachStaticIp'
    { staticIpName = pStaticIpName_,
      instanceName = pInstanceName_
    }

-- | The name of the static IP.
attachStaticIp_staticIpName :: Lens.Lens' AttachStaticIp Prelude.Text
attachStaticIp_staticIpName = Lens.lens (\AttachStaticIp' {staticIpName} -> staticIpName) (\s@AttachStaticIp' {} a -> s {staticIpName = a} :: AttachStaticIp)

-- | The instance name to which you want to attach the static IP address.
attachStaticIp_instanceName :: Lens.Lens' AttachStaticIp Prelude.Text
attachStaticIp_instanceName = Lens.lens (\AttachStaticIp' {instanceName} -> instanceName) (\s@AttachStaticIp' {} a -> s {instanceName = a} :: AttachStaticIp)

instance Core.AWSRequest AttachStaticIp where
  type
    AWSResponse AttachStaticIp =
      AttachStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachStaticIpResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachStaticIp

instance Prelude.NFData AttachStaticIp

instance Core.ToHeaders AttachStaticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.AttachStaticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AttachStaticIp where
  toJSON AttachStaticIp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("staticIpName" Core..= staticIpName),
            Prelude.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath AttachStaticIp where
  toPath = Prelude.const "/"

instance Core.ToQuery AttachStaticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachStaticIpResponse' smart constructor.
data AttachStaticIpResponse = AttachStaticIpResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'attachStaticIpResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'attachStaticIpResponse_httpStatus' - The response's http status code.
newAttachStaticIpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachStaticIpResponse
newAttachStaticIpResponse pHttpStatus_ =
  AttachStaticIpResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
attachStaticIpResponse_operations :: Lens.Lens' AttachStaticIpResponse (Prelude.Maybe [Operation])
attachStaticIpResponse_operations = Lens.lens (\AttachStaticIpResponse' {operations} -> operations) (\s@AttachStaticIpResponse' {} a -> s {operations = a} :: AttachStaticIpResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
attachStaticIpResponse_httpStatus :: Lens.Lens' AttachStaticIpResponse Prelude.Int
attachStaticIpResponse_httpStatus = Lens.lens (\AttachStaticIpResponse' {httpStatus} -> httpStatus) (\s@AttachStaticIpResponse' {} a -> s {httpStatus = a} :: AttachStaticIpResponse)

instance Prelude.NFData AttachStaticIpResponse
