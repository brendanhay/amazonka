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
-- Module      : Amazonka.Lightsail.AttachStaticIp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a static IP address to a specific Amazon Lightsail instance.
module Amazonka.Lightsail.AttachStaticIp
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachStaticIpResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachStaticIp where
  hashWithSalt _salt AttachStaticIp' {..} =
    _salt
      `Prelude.hashWithSalt` staticIpName
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData AttachStaticIp where
  rnf AttachStaticIp' {..} =
    Prelude.rnf staticIpName
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders AttachStaticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.AttachStaticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AttachStaticIp where
  toJSON AttachStaticIp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("staticIpName" Data..= staticIpName),
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath AttachStaticIp where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachStaticIp where
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
attachStaticIpResponse_operations = Lens.lens (\AttachStaticIpResponse' {operations} -> operations) (\s@AttachStaticIpResponse' {} a -> s {operations = a} :: AttachStaticIpResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
attachStaticIpResponse_httpStatus :: Lens.Lens' AttachStaticIpResponse Prelude.Int
attachStaticIpResponse_httpStatus = Lens.lens (\AttachStaticIpResponse' {httpStatus} -> httpStatus) (\s@AttachStaticIpResponse' {} a -> s {httpStatus = a} :: AttachStaticIpResponse)

instance Prelude.NFData AttachStaticIpResponse where
  rnf AttachStaticIpResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
