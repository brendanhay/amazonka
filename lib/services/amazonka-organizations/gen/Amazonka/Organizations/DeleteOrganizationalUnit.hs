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
-- Module      : Amazonka.Organizations.DeleteOrganizationalUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an organizational unit (OU) from a root or another OU. You must
-- first remove all accounts and child OUs from the OU that you want to
-- delete.
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.DeleteOrganizationalUnit
  ( -- * Creating a Request
    DeleteOrganizationalUnit (..),
    newDeleteOrganizationalUnit,

    -- * Request Lenses
    deleteOrganizationalUnit_organizationalUnitId,

    -- * Destructuring the Response
    DeleteOrganizationalUnitResponse (..),
    newDeleteOrganizationalUnitResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOrganizationalUnit' smart constructor.
data DeleteOrganizationalUnit = DeleteOrganizationalUnit'
  { -- | The unique identifier (ID) of the organizational unit that you want to
    -- delete. You can get the ID from the ListOrganizationalUnitsForParent
    -- operation.
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
-- Create a value of 'DeleteOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitId', 'deleteOrganizationalUnit_organizationalUnitId' - The unique identifier (ID) of the organizational unit that you want to
-- delete. You can get the ID from the ListOrganizationalUnitsForParent
-- operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
newDeleteOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Prelude.Text ->
  DeleteOrganizationalUnit
newDeleteOrganizationalUnit pOrganizationalUnitId_ =
  DeleteOrganizationalUnit'
    { organizationalUnitId =
        pOrganizationalUnitId_
    }

-- | The unique identifier (ID) of the organizational unit that you want to
-- delete. You can get the ID from the ListOrganizationalUnitsForParent
-- operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an
-- organizational unit ID string requires \"ou-\" followed by from 4 to 32
-- lowercase letters or digits (the ID of the root that contains the OU).
-- This string is followed by a second \"-\" dash and from 8 to 32
-- additional lowercase letters or digits.
deleteOrganizationalUnit_organizationalUnitId :: Lens.Lens' DeleteOrganizationalUnit Prelude.Text
deleteOrganizationalUnit_organizationalUnitId = Lens.lens (\DeleteOrganizationalUnit' {organizationalUnitId} -> organizationalUnitId) (\s@DeleteOrganizationalUnit' {} a -> s {organizationalUnitId = a} :: DeleteOrganizationalUnit)

instance Core.AWSRequest DeleteOrganizationalUnit where
  type
    AWSResponse DeleteOrganizationalUnit =
      DeleteOrganizationalUnitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteOrganizationalUnitResponse'

instance Prelude.Hashable DeleteOrganizationalUnit where
  hashWithSalt _salt DeleteOrganizationalUnit' {..} =
    _salt `Prelude.hashWithSalt` organizationalUnitId

instance Prelude.NFData DeleteOrganizationalUnit where
  rnf DeleteOrganizationalUnit' {..} =
    Prelude.rnf organizationalUnitId

instance Data.ToHeaders DeleteOrganizationalUnit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DeleteOrganizationalUnit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteOrganizationalUnit where
  toJSON DeleteOrganizationalUnit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationalUnitId"
                  Data..= organizationalUnitId
              )
          ]
      )

instance Data.ToPath DeleteOrganizationalUnit where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteOrganizationalUnit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationalUnitResponse' smart constructor.
data DeleteOrganizationalUnitResponse = DeleteOrganizationalUnitResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationalUnitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOrganizationalUnitResponse ::
  DeleteOrganizationalUnitResponse
newDeleteOrganizationalUnitResponse =
  DeleteOrganizationalUnitResponse'

instance
  Prelude.NFData
    DeleteOrganizationalUnitResponse
  where
  rnf _ = ()
