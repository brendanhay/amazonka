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
import qualified Network.AWS.Prelude as Prelude
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
    organizationalUnitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
describeOrganizationalUnit_organizationalUnitId :: Lens.Lens' DescribeOrganizationalUnit Prelude.Text
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
            Prelude.<$> (x Core..?> "OrganizationalUnit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOrganizationalUnit

instance Prelude.NFData DescribeOrganizationalUnit

instance Core.ToHeaders DescribeOrganizationalUnit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeOrganizationalUnit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeOrganizationalUnit where
  toJSON DescribeOrganizationalUnit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationalUnitId"
                  Core..= organizationalUnitId
              )
          ]
      )

instance Core.ToPath DescribeOrganizationalUnit where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeOrganizationalUnit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationalUnitResponse' smart constructor.
data DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse'
  { -- | A structure that contains details about the specified OU.
    organizationalUnit :: Prelude.Maybe OrganizationalUnit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeOrganizationalUnitResponse
newDescribeOrganizationalUnitResponse pHttpStatus_ =
  DescribeOrganizationalUnitResponse'
    { organizationalUnit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the specified OU.
describeOrganizationalUnitResponse_organizationalUnit :: Lens.Lens' DescribeOrganizationalUnitResponse (Prelude.Maybe OrganizationalUnit)
describeOrganizationalUnitResponse_organizationalUnit = Lens.lens (\DescribeOrganizationalUnitResponse' {organizationalUnit} -> organizationalUnit) (\s@DescribeOrganizationalUnitResponse' {} a -> s {organizationalUnit = a} :: DescribeOrganizationalUnitResponse)

-- | The response's http status code.
describeOrganizationalUnitResponse_httpStatus :: Lens.Lens' DescribeOrganizationalUnitResponse Prelude.Int
describeOrganizationalUnitResponse_httpStatus = Lens.lens (\DescribeOrganizationalUnitResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationalUnitResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationalUnitResponse)

instance
  Prelude.NFData
    DescribeOrganizationalUnitResponse
