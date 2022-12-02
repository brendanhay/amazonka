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
-- Module      : Amazonka.Organizations.DescribeOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the organization that the user\'s account
-- belongs to.
--
-- This operation can be called from any account in the organization.
--
-- Even if a policy type is shown as available in the organization, you can
-- disable it separately at the root level with DisablePolicyType. Use
-- ListRoots to see the status of policy types for a specified root.
module Amazonka.Organizations.DescribeOrganization
  ( -- * Creating a Request
    DescribeOrganization (..),
    newDescribeOrganization,

    -- * Destructuring the Response
    DescribeOrganizationResponse (..),
    newDescribeOrganizationResponse,

    -- * Response Lenses
    describeOrganizationResponse_organization,
    describeOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganization' smart constructor.
data DescribeOrganization = DescribeOrganization'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeOrganization ::
  DescribeOrganization
newDescribeOrganization = DescribeOrganization'

instance Core.AWSRequest DescribeOrganization where
  type
    AWSResponse DescribeOrganization =
      DescribeOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Prelude.<$> (x Data..?> "Organization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOrganization where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeOrganization where
  rnf _ = ()

instance Data.ToHeaders DescribeOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DescribeOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOrganization where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeOrganization where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { -- | A structure that contains information about the organization.
    --
    -- The @AvailablePolicyTypes@ part of the response is deprecated, and you
    -- shouldn\'t use it in your apps. It doesn\'t include any policy type
    -- supported by Organizations other than SCPs. To determine which policy
    -- types are enabled in your organization, use the @ ListRoots @ operation.
    organization :: Prelude.Maybe Organization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organization', 'describeOrganizationResponse_organization' - A structure that contains information about the organization.
--
-- The @AvailablePolicyTypes@ part of the response is deprecated, and you
-- shouldn\'t use it in your apps. It doesn\'t include any policy type
-- supported by Organizations other than SCPs. To determine which policy
-- types are enabled in your organization, use the @ ListRoots @ operation.
--
-- 'httpStatus', 'describeOrganizationResponse_httpStatus' - The response's http status code.
newDescribeOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationResponse
newDescribeOrganizationResponse pHttpStatus_ =
  DescribeOrganizationResponse'
    { organization =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains information about the organization.
--
-- The @AvailablePolicyTypes@ part of the response is deprecated, and you
-- shouldn\'t use it in your apps. It doesn\'t include any policy type
-- supported by Organizations other than SCPs. To determine which policy
-- types are enabled in your organization, use the @ ListRoots @ operation.
describeOrganizationResponse_organization :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Organization)
describeOrganizationResponse_organization = Lens.lens (\DescribeOrganizationResponse' {organization} -> organization) (\s@DescribeOrganizationResponse' {} a -> s {organization = a} :: DescribeOrganizationResponse)

-- | The response's http status code.
describeOrganizationResponse_httpStatus :: Lens.Lens' DescribeOrganizationResponse Prelude.Int
describeOrganizationResponse_httpStatus = Lens.lens (\DescribeOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationResponse)

instance Prelude.NFData DescribeOrganizationResponse where
  rnf DescribeOrganizationResponse' {..} =
    Prelude.rnf organization
      `Prelude.seq` Prelude.rnf httpStatus
