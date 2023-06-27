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
-- Module      : Amazonka.RAM.EnableSharingWithAwsOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables resource sharing within your organization in Organizations. This
-- operation creates a service-linked role called
-- @AWSServiceRoleForResourceAccessManager@ that has the IAM managed policy
-- named AWSResourceAccessManagerServiceRolePolicy attached. This role
-- permits RAM to retrieve information about the organization and its
-- structure. This lets you share resources with all of the accounts in the
-- calling account\'s organization by specifying the organization ID, or
-- all of the accounts in an organizational unit (OU) by specifying the OU
-- ID. Until you enable sharing within the organization, you can specify
-- only individual Amazon Web Services accounts, or for supported resource
-- types, IAM roles and users.
--
-- You must call this operation from an IAM role or user in the
-- organization\'s management account.
module Amazonka.RAM.EnableSharingWithAwsOrganization
  ( -- * Creating a Request
    EnableSharingWithAwsOrganization (..),
    newEnableSharingWithAwsOrganization,

    -- * Destructuring the Response
    EnableSharingWithAwsOrganizationResponse (..),
    newEnableSharingWithAwsOrganizationResponse,

    -- * Response Lenses
    enableSharingWithAwsOrganizationResponse_returnValue,
    enableSharingWithAwsOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableSharingWithAwsOrganization' smart constructor.
data EnableSharingWithAwsOrganization = EnableSharingWithAwsOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSharingWithAwsOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableSharingWithAwsOrganization ::
  EnableSharingWithAwsOrganization
newEnableSharingWithAwsOrganization =
  EnableSharingWithAwsOrganization'

instance
  Core.AWSRequest
    EnableSharingWithAwsOrganization
  where
  type
    AWSResponse EnableSharingWithAwsOrganization =
      EnableSharingWithAwsOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableSharingWithAwsOrganizationResponse'
            Prelude.<$> (x Data..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableSharingWithAwsOrganization
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    EnableSharingWithAwsOrganization
  where
  rnf _ = ()

instance
  Data.ToHeaders
    EnableSharingWithAwsOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableSharingWithAwsOrganization where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath EnableSharingWithAwsOrganization where
  toPath =
    Prelude.const "/enablesharingwithawsorganization"

instance
  Data.ToQuery
    EnableSharingWithAwsOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableSharingWithAwsOrganizationResponse' smart constructor.
data EnableSharingWithAwsOrganizationResponse = EnableSharingWithAwsOrganizationResponse'
  { -- | A return value of @true@ indicates that the request succeeded. A value
    -- of @false@ indicates that the request failed.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSharingWithAwsOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'enableSharingWithAwsOrganizationResponse_returnValue' - A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
--
-- 'httpStatus', 'enableSharingWithAwsOrganizationResponse_httpStatus' - The response's http status code.
newEnableSharingWithAwsOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSharingWithAwsOrganizationResponse
newEnableSharingWithAwsOrganizationResponse
  pHttpStatus_ =
    EnableSharingWithAwsOrganizationResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
enableSharingWithAwsOrganizationResponse_returnValue :: Lens.Lens' EnableSharingWithAwsOrganizationResponse (Prelude.Maybe Prelude.Bool)
enableSharingWithAwsOrganizationResponse_returnValue = Lens.lens (\EnableSharingWithAwsOrganizationResponse' {returnValue} -> returnValue) (\s@EnableSharingWithAwsOrganizationResponse' {} a -> s {returnValue = a} :: EnableSharingWithAwsOrganizationResponse)

-- | The response's http status code.
enableSharingWithAwsOrganizationResponse_httpStatus :: Lens.Lens' EnableSharingWithAwsOrganizationResponse Prelude.Int
enableSharingWithAwsOrganizationResponse_httpStatus = Lens.lens (\EnableSharingWithAwsOrganizationResponse' {httpStatus} -> httpStatus) (\s@EnableSharingWithAwsOrganizationResponse' {} a -> s {httpStatus = a} :: EnableSharingWithAwsOrganizationResponse)

instance
  Prelude.NFData
    EnableSharingWithAwsOrganizationResponse
  where
  rnf EnableSharingWithAwsOrganizationResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
