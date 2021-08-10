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
-- Module      : Network.AWS.Lightsail.ReleaseStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific static IP from your account.
module Network.AWS.Lightsail.ReleaseStaticIp
  ( -- * Creating a Request
    ReleaseStaticIp (..),
    newReleaseStaticIp,

    -- * Request Lenses
    releaseStaticIp_staticIpName,

    -- * Destructuring the Response
    ReleaseStaticIpResponse (..),
    newReleaseStaticIpResponse,

    -- * Response Lenses
    releaseStaticIpResponse_operations,
    releaseStaticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReleaseStaticIp' smart constructor.
data ReleaseStaticIp = ReleaseStaticIp'
  { -- | The name of the static IP to delete.
    staticIpName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'releaseStaticIp_staticIpName' - The name of the static IP to delete.
newReleaseStaticIp ::
  -- | 'staticIpName'
  Prelude.Text ->
  ReleaseStaticIp
newReleaseStaticIp pStaticIpName_ =
  ReleaseStaticIp' {staticIpName = pStaticIpName_}

-- | The name of the static IP to delete.
releaseStaticIp_staticIpName :: Lens.Lens' ReleaseStaticIp Prelude.Text
releaseStaticIp_staticIpName = Lens.lens (\ReleaseStaticIp' {staticIpName} -> staticIpName) (\s@ReleaseStaticIp' {} a -> s {staticIpName = a} :: ReleaseStaticIp)

instance Core.AWSRequest ReleaseStaticIp where
  type
    AWSResponse ReleaseStaticIp =
      ReleaseStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReleaseStaticIpResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReleaseStaticIp

instance Prelude.NFData ReleaseStaticIp

instance Core.ToHeaders ReleaseStaticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.ReleaseStaticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ReleaseStaticIp where
  toJSON ReleaseStaticIp' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("staticIpName" Core..= staticIpName)]
      )

instance Core.ToPath ReleaseStaticIp where
  toPath = Prelude.const "/"

instance Core.ToQuery ReleaseStaticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReleaseStaticIpResponse' smart constructor.
data ReleaseStaticIpResponse = ReleaseStaticIpResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'releaseStaticIpResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'releaseStaticIpResponse_httpStatus' - The response's http status code.
newReleaseStaticIpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReleaseStaticIpResponse
newReleaseStaticIpResponse pHttpStatus_ =
  ReleaseStaticIpResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
releaseStaticIpResponse_operations :: Lens.Lens' ReleaseStaticIpResponse (Prelude.Maybe [Operation])
releaseStaticIpResponse_operations = Lens.lens (\ReleaseStaticIpResponse' {operations} -> operations) (\s@ReleaseStaticIpResponse' {} a -> s {operations = a} :: ReleaseStaticIpResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
releaseStaticIpResponse_httpStatus :: Lens.Lens' ReleaseStaticIpResponse Prelude.Int
releaseStaticIpResponse_httpStatus = Lens.lens (\ReleaseStaticIpResponse' {httpStatus} -> httpStatus) (\s@ReleaseStaticIpResponse' {} a -> s {httpStatus = a} :: ReleaseStaticIpResponse)

instance Prelude.NFData ReleaseStaticIpResponse
