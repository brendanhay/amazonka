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
-- Module      : Network.AWS.Organizations.DescribeOrganizationalUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an organizational unit (OU).
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
module Network.AWS.Organizations.DescribeOrganizationalUnit
  ( -- * Creating a Request
    DescribeOrganizationalUnit (..),
    newDescribeOrganizationalUnit,

    -- * Request Lenses
    describeOrganizationalUnit_organizationalUnitId,

    -- * Destructuring the Response
    DescribeOrganizationalUnitResponse (..),
    newDescribeOrganizationalUnitResponse,

    -- * Response Lenses
    describeOrganizationalUnitResponse_organizationalUnit,
    describeOrganizationalUnitResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOrganizationalUnit' smart constructor.
data DescribeOrganizationalUnit = DescribeOrganizationalUnit'
  { -- | The unique identifier (ID) of the organizational unit that you want
    -- details about. You can get the ID from the
    -- ListOrganizationalUnitsForParent operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an
    -- organizational unit ID string requires \"ou-\" followed by from 4 to 32
    -- lowercase letters or digits (the ID of the root that contains the OU).
    -- This string is followed by a second \"-\" dash and from 8 to 32
    -- additional lowercase letters or digits.
    organizationalUnitId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitId', 'describeOrganizationalUnit_organizationalUnitId' - The unique identifier (ID) of the organizational unit that you want
-- details about. You can get the ID from the
-- ListOrganizationalUnitsForParent operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
newDescribeOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Core.Text ->
  DescribeOrganizationalUnit
newDescribeOrganizationalUnit pOrganizationalUnitId_ =
  DescribeOrganizationalUnit'
    { organizationalUnitId =
        pOrganizationalUnitId_
    }

-- | The unique identifier (ID) of the organizational unit that you want
-- details about. You can get the ID from the
-- ListOrganizationalUnitsForParent operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
describeOrganizationalUnit_organizationalUnitId :: Lens.Lens' DescribeOrganizationalUnit Core.Text
describeOrganizationalUnit_organizationalUnitId = Lens.lens (\DescribeOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@DescribeOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: DescribeOrganizationalUnit)

instance Core.AWSRequest DescribeOrganizationalUnit where
  type
    AWSResponse DescribeOrganizationalUnit =
      DescribeOrganizationalUnitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationalUnitResponse'
            Core.<$> (x Core..?> "OrganizationalUnit")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeOrganizationalUnit

instance Core.NFData DescribeOrganizationalUnit

instance Core.ToHeaders DescribeOrganizationalUnit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeOrganizationalUnit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeOrganizationalUnit where
  toJSON DescribeOrganizationalUnit' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OrganizationalUnitId"
                  Core..= organizationalUnitId
              )
          ]
      )

instance Core.ToPath DescribeOrganizationalUnit where
  toPath = Core.const "/"

instance Core.ToQuery DescribeOrganizationalUnit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeOrganizationalUnitResponse' smart constructor.
data DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse'
  { -- | A structure that contains details about the specified OU.
    organizationalUnit :: Core.Maybe OrganizationalUnit,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrganizationalUnitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnit', 'describeOrganizationalUnitResponse_organizationalUnit' - A structure that contains details about the specified OU.
--
-- 'httpStatus', 'describeOrganizationalUnitResponse_httpStatus' - The response's http status code.
newDescribeOrganizationalUnitResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOrganizationalUnitResponse
newDescribeOrganizationalUnitResponse pHttpStatus_ =
  DescribeOrganizationalUnitResponse'
    { organizationalUnit =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the specified OU.
describeOrganizationalUnitResponse_organizationalUnit :: Lens.Lens' DescribeOrganizationalUnitResponse (Core.Maybe OrganizationalUnit)
describeOrganizationalUnitResponse_organizationalUnit = Lens.lens (\DescribeOrganizationalUnitResponse' {organizationalUnit} -> organizationalUnit) (\s@DescribeOrganizationalUnitResponse' {} a -> s {organizationalUnit = a} :: DescribeOrganizationalUnitResponse)

-- | The response's http status code.
describeOrganizationalUnitResponse_httpStatus :: Lens.Lens' DescribeOrganizationalUnitResponse Core.Int
describeOrganizationalUnitResponse_httpStatus = Lens.lens (\DescribeOrganizationalUnitResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationalUnitResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationalUnitResponse)

instance
  Core.NFData
    DescribeOrganizationalUnitResponse
