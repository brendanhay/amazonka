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
-- Module      : Network.AWS.Organizations.DescribeOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Organizations.DescribeOrganization
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOrganization' smart constructor.
data DescribeOrganization = DescribeOrganization'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Core.<$> (x Core..?> "Organization")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeOrganization

instance Core.NFData DescribeOrganization

instance Core.ToHeaders DescribeOrganization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeOrganization where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeOrganization where
  toPath = Core.const "/"

instance Core.ToQuery DescribeOrganization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { -- | A structure that contains information about the organization.
    --
    -- The @AvailablePolicyTypes@ part of the response is deprecated, and you
    -- shouldn\'t use it in your apps. It doesn\'t include any policy type
    -- supported by Organizations other than SCPs. To determine which policy
    -- types are enabled in your organization, use the @ ListRoots @ operation.
    organization :: Core.Maybe Organization,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeOrganizationResponse
newDescribeOrganizationResponse pHttpStatus_ =
  DescribeOrganizationResponse'
    { organization =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains information about the organization.
--
-- The @AvailablePolicyTypes@ part of the response is deprecated, and you
-- shouldn\'t use it in your apps. It doesn\'t include any policy type
-- supported by Organizations other than SCPs. To determine which policy
-- types are enabled in your organization, use the @ ListRoots @ operation.
describeOrganizationResponse_organization :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Organization)
describeOrganizationResponse_organization = Lens.lens (\DescribeOrganizationResponse' {organization} -> organization) (\s@DescribeOrganizationResponse' {} a -> s {organization = a} :: DescribeOrganizationResponse)

-- | The response's http status code.
describeOrganizationResponse_httpStatus :: Lens.Lens' DescribeOrganizationResponse Core.Int
describeOrganizationResponse_httpStatus = Lens.lens (\DescribeOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationResponse)

instance Core.NFData DescribeOrganizationResponse
