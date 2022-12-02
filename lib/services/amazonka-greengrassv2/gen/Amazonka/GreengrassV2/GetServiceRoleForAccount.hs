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
-- Module      : Amazonka.GreengrassV2.GetServiceRoleForAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the service role associated with IoT Greengrass for your Amazon Web
-- Services account in this Amazon Web Services Region. IoT Greengrass uses
-- this role to verify the identity of client devices and manage core
-- device connectivity information. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-service-role.html Greengrass service role>
-- in the /IoT Greengrass Version 2 Developer Guide/.
module Amazonka.GreengrassV2.GetServiceRoleForAccount
  ( -- * Creating a Request
    GetServiceRoleForAccount (..),
    newGetServiceRoleForAccount,

    -- * Destructuring the Response
    GetServiceRoleForAccountResponse (..),
    newGetServiceRoleForAccountResponse,

    -- * Response Lenses
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceRoleForAccount' smart constructor.
data GetServiceRoleForAccount = GetServiceRoleForAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceRoleForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetServiceRoleForAccount ::
  GetServiceRoleForAccount
newGetServiceRoleForAccount =
  GetServiceRoleForAccount'

instance Core.AWSRequest GetServiceRoleForAccount where
  type
    AWSResponse GetServiceRoleForAccount =
      GetServiceRoleForAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceRoleForAccountResponse'
            Prelude.<$> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceRoleForAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetServiceRoleForAccount where
  rnf _ = ()

instance Data.ToHeaders GetServiceRoleForAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetServiceRoleForAccount where
  toPath = Prelude.const "/greengrass/servicerole"

instance Data.ToQuery GetServiceRoleForAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceRoleForAccountResponse' smart constructor.
data GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse'
  { -- | The ARN of the service role that is associated with IoT Greengrass for
    -- your Amazon Web Services account in this Amazon Web Services Region.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the service role was associated with IoT Greengrass for
    -- your Amazon Web Services account in this Amazon Web Services Region.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceRoleForAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getServiceRoleForAccountResponse_roleArn' - The ARN of the service role that is associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
--
-- 'associatedAt', 'getServiceRoleForAccountResponse_associatedAt' - The time when the service role was associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
--
-- 'httpStatus', 'getServiceRoleForAccountResponse_httpStatus' - The response's http status code.
newGetServiceRoleForAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceRoleForAccountResponse
newGetServiceRoleForAccountResponse pHttpStatus_ =
  GetServiceRoleForAccountResponse'
    { roleArn =
        Prelude.Nothing,
      associatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the service role that is associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
getServiceRoleForAccountResponse_roleArn :: Lens.Lens' GetServiceRoleForAccountResponse (Prelude.Maybe Prelude.Text)
getServiceRoleForAccountResponse_roleArn = Lens.lens (\GetServiceRoleForAccountResponse' {roleArn} -> roleArn) (\s@GetServiceRoleForAccountResponse' {} a -> s {roleArn = a} :: GetServiceRoleForAccountResponse)

-- | The time when the service role was associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
getServiceRoleForAccountResponse_associatedAt :: Lens.Lens' GetServiceRoleForAccountResponse (Prelude.Maybe Prelude.Text)
getServiceRoleForAccountResponse_associatedAt = Lens.lens (\GetServiceRoleForAccountResponse' {associatedAt} -> associatedAt) (\s@GetServiceRoleForAccountResponse' {} a -> s {associatedAt = a} :: GetServiceRoleForAccountResponse)

-- | The response's http status code.
getServiceRoleForAccountResponse_httpStatus :: Lens.Lens' GetServiceRoleForAccountResponse Prelude.Int
getServiceRoleForAccountResponse_httpStatus = Lens.lens (\GetServiceRoleForAccountResponse' {httpStatus} -> httpStatus) (\s@GetServiceRoleForAccountResponse' {} a -> s {httpStatus = a} :: GetServiceRoleForAccountResponse)

instance
  Prelude.NFData
    GetServiceRoleForAccountResponse
  where
  rnf GetServiceRoleForAccountResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf associatedAt
      `Prelude.seq` Prelude.rnf httpStatus
