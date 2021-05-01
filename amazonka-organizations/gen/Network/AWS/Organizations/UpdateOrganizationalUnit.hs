{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.UpdateOrganizationalUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames the specified organizational unit (OU). The ID and ARN don\'t
-- change. The child OUs and accounts remain in place, and any attached
-- policies of the OU remain attached.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.UpdateOrganizationalUnit
  ( -- * Creating a Request
    UpdateOrganizationalUnit (..),
    newUpdateOrganizationalUnit,

    -- * Request Lenses
    updateOrganizationalUnit_name,
    updateOrganizationalUnit_organizationalUnitId,

    -- * Destructuring the Response
    UpdateOrganizationalUnitResponse (..),
    newUpdateOrganizationalUnitResponse,

    -- * Response Lenses
    updateOrganizationalUnitResponse_organizationalUnit,
    updateOrganizationalUnitResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateOrganizationalUnit' smart constructor.
data UpdateOrganizationalUnit = UpdateOrganizationalUnit'
  { -- | The new name that you want to assign to the OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the OU that you want to rename. You can
    -- get the ID from the ListOrganizationalUnitsForParent operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an
    -- organizational unit ID string requires \"ou-\" followed by from 4 to 32
    -- lowercase letters or digits (the ID of the root that contains the OU).
    -- This string is followed by a second \"-\" dash and from 8 to 32
    -- additional lowercase letters or digits.
    organizationalUnitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateOrganizationalUnit_name' - The new name that you want to assign to the OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'organizationalUnitId', 'updateOrganizationalUnit_organizationalUnitId' - The unique identifier (ID) of the OU that you want to rename. You can
-- get the ID from the ListOrganizationalUnitsForParent operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
newUpdateOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Prelude.Text ->
  UpdateOrganizationalUnit
newUpdateOrganizationalUnit pOrganizationalUnitId_ =
  UpdateOrganizationalUnit'
    { name = Prelude.Nothing,
      organizationalUnitId = pOrganizationalUnitId_
    }

-- | The new name that you want to assign to the OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
updateOrganizationalUnit_name :: Lens.Lens' UpdateOrganizationalUnit (Prelude.Maybe Prelude.Text)
updateOrganizationalUnit_name = Lens.lens (\UpdateOrganizationalUnit' {name} -> name) (\s@UpdateOrganizationalUnit' {} a -> s {name = a} :: UpdateOrganizationalUnit)

-- | The unique identifier (ID) of the OU that you want to rename. You can
-- get the ID from the ListOrganizationalUnitsForParent operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
updateOrganizationalUnit_organizationalUnitId :: Lens.Lens' UpdateOrganizationalUnit Prelude.Text
updateOrganizationalUnit_organizationalUnitId = Lens.lens (\UpdateOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@UpdateOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: UpdateOrganizationalUnit)

instance Prelude.AWSRequest UpdateOrganizationalUnit where
  type
    Rs UpdateOrganizationalUnit =
      UpdateOrganizationalUnitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOrganizationalUnitResponse'
            Prelude.<$> (x Prelude..?> "OrganizationalUnit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOrganizationalUnit

instance Prelude.NFData UpdateOrganizationalUnit

instance Prelude.ToHeaders UpdateOrganizationalUnit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.UpdateOrganizationalUnit" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateOrganizationalUnit where
  toJSON UpdateOrganizationalUnit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            Prelude.Just
              ( "OrganizationalUnitId"
                  Prelude..= organizationalUnitId
              )
          ]
      )

instance Prelude.ToPath UpdateOrganizationalUnit where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateOrganizationalUnit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOrganizationalUnitResponse' smart constructor.
data UpdateOrganizationalUnitResponse = UpdateOrganizationalUnitResponse'
  { -- | A structure that contains the details about the specified OU, including
    -- its new name.
    organizationalUnit :: Prelude.Maybe OrganizationalUnit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationalUnitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnit', 'updateOrganizationalUnitResponse_organizationalUnit' - A structure that contains the details about the specified OU, including
-- its new name.
--
-- 'httpStatus', 'updateOrganizationalUnitResponse_httpStatus' - The response's http status code.
newUpdateOrganizationalUnitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOrganizationalUnitResponse
newUpdateOrganizationalUnitResponse pHttpStatus_ =
  UpdateOrganizationalUnitResponse'
    { organizationalUnit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the details about the specified OU, including
-- its new name.
updateOrganizationalUnitResponse_organizationalUnit :: Lens.Lens' UpdateOrganizationalUnitResponse (Prelude.Maybe OrganizationalUnit)
updateOrganizationalUnitResponse_organizationalUnit = Lens.lens (\UpdateOrganizationalUnitResponse' {organizationalUnit} -> organizationalUnit) (\s@UpdateOrganizationalUnitResponse' {} a -> s {organizationalUnit = a} :: UpdateOrganizationalUnitResponse)

-- | The response's http status code.
updateOrganizationalUnitResponse_httpStatus :: Lens.Lens' UpdateOrganizationalUnitResponse Prelude.Int
updateOrganizationalUnitResponse_httpStatus = Lens.lens (\UpdateOrganizationalUnitResponse' {httpStatus} -> httpStatus) (\s@UpdateOrganizationalUnitResponse' {} a -> s {httpStatus = a} :: UpdateOrganizationalUnitResponse)

instance
  Prelude.NFData
    UpdateOrganizationalUnitResponse
