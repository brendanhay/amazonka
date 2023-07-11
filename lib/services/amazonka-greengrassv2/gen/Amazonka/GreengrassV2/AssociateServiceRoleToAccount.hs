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
-- Module      : Amazonka.GreengrassV2.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Greengrass service role with IoT Greengrass for your Amazon
-- Web Services account in this Amazon Web Services Region. IoT Greengrass
-- uses this role to verify the identity of client devices and manage core
-- device connectivity information. The role must include the
-- <https://console.aws.amazon.com/iam/home#/policies/arn:awsiam::aws:policy/service-role/AWSGreengrassResourceAccessRolePolicy AWSGreengrassResourceAccessRolePolicy>
-- managed policy or a custom policy that defines equivalent permissions
-- for the IoT Greengrass features that you use. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-service-role.html Greengrass service role>
-- in the /IoT Greengrass Version 2 Developer Guide/.
module Amazonka.GreengrassV2.AssociateServiceRoleToAccount
  ( -- * Creating a Request
    AssociateServiceRoleToAccount (..),
    newAssociateServiceRoleToAccount,

    -- * Request Lenses
    associateServiceRoleToAccount_roleArn,

    -- * Destructuring the Response
    AssociateServiceRoleToAccountResponse (..),
    newAssociateServiceRoleToAccountResponse,

    -- * Response Lenses
    associateServiceRoleToAccountResponse_associatedAt,
    associateServiceRoleToAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateServiceRoleToAccount' smart constructor.
data AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { -- | The Amazon Resource Name (ARN) of the service role to associate with IoT
    -- Greengrass for your Amazon Web Services account in this Amazon Web
    -- Services Region.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateServiceRoleToAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'associateServiceRoleToAccount_roleArn' - The Amazon Resource Name (ARN) of the service role to associate with IoT
-- Greengrass for your Amazon Web Services account in this Amazon Web
-- Services Region.
newAssociateServiceRoleToAccount ::
  -- | 'roleArn'
  Prelude.Text ->
  AssociateServiceRoleToAccount
newAssociateServiceRoleToAccount pRoleArn_ =
  AssociateServiceRoleToAccount' {roleArn = pRoleArn_}

-- | The Amazon Resource Name (ARN) of the service role to associate with IoT
-- Greengrass for your Amazon Web Services account in this Amazon Web
-- Services Region.
associateServiceRoleToAccount_roleArn :: Lens.Lens' AssociateServiceRoleToAccount Prelude.Text
associateServiceRoleToAccount_roleArn = Lens.lens (\AssociateServiceRoleToAccount' {roleArn} -> roleArn) (\s@AssociateServiceRoleToAccount' {} a -> s {roleArn = a} :: AssociateServiceRoleToAccount)

instance
  Core.AWSRequest
    AssociateServiceRoleToAccount
  where
  type
    AWSResponse AssociateServiceRoleToAccount =
      AssociateServiceRoleToAccountResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateServiceRoleToAccountResponse'
            Prelude.<$> (x Data..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateServiceRoleToAccount
  where
  hashWithSalt _salt AssociateServiceRoleToAccount' {..} =
    _salt `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AssociateServiceRoleToAccount where
  rnf AssociateServiceRoleToAccount' {..} =
    Prelude.rnf roleArn

instance Data.ToHeaders AssociateServiceRoleToAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AssociateServiceRoleToAccount where
  toJSON AssociateServiceRoleToAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Data..= roleArn)]
      )

instance Data.ToPath AssociateServiceRoleToAccount where
  toPath = Prelude.const "/greengrass/servicerole"

instance Data.ToQuery AssociateServiceRoleToAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { -- | The time when the service role was associated with IoT Greengrass for
    -- your Amazon Web Services account in this Amazon Web Services Region.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateServiceRoleToAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAt', 'associateServiceRoleToAccountResponse_associatedAt' - The time when the service role was associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
--
-- 'httpStatus', 'associateServiceRoleToAccountResponse_httpStatus' - The response's http status code.
newAssociateServiceRoleToAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateServiceRoleToAccountResponse
newAssociateServiceRoleToAccountResponse pHttpStatus_ =
  AssociateServiceRoleToAccountResponse'
    { associatedAt =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the service role was associated with IoT Greengrass for
-- your Amazon Web Services account in this Amazon Web Services Region.
associateServiceRoleToAccountResponse_associatedAt :: Lens.Lens' AssociateServiceRoleToAccountResponse (Prelude.Maybe Prelude.Text)
associateServiceRoleToAccountResponse_associatedAt = Lens.lens (\AssociateServiceRoleToAccountResponse' {associatedAt} -> associatedAt) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {associatedAt = a} :: AssociateServiceRoleToAccountResponse)

-- | The response's http status code.
associateServiceRoleToAccountResponse_httpStatus :: Lens.Lens' AssociateServiceRoleToAccountResponse Prelude.Int
associateServiceRoleToAccountResponse_httpStatus = Lens.lens (\AssociateServiceRoleToAccountResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {httpStatus = a} :: AssociateServiceRoleToAccountResponse)

instance
  Prelude.NFData
    AssociateServiceRoleToAccountResponse
  where
  rnf AssociateServiceRoleToAccountResponse' {..} =
    Prelude.rnf associatedAt
      `Prelude.seq` Prelude.rnf httpStatus
