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
-- Module      : Network.AWS.Organizations.DeleteOrganizationalUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an organizational unit (OU) from a root or another OU. You must
-- first remove all accounts and child OUs from the OU that you want to
-- delete.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.DeleteOrganizationalUnit
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteOrganizationalUnit where
  type
    Rs DeleteOrganizationalUnit =
      DeleteOrganizationalUnitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteOrganizationalUnitResponse'

instance Prelude.Hashable DeleteOrganizationalUnit

instance Prelude.NFData DeleteOrganizationalUnit

instance Prelude.ToHeaders DeleteOrganizationalUnit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.DeleteOrganizationalUnit" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteOrganizationalUnit where
  toJSON DeleteOrganizationalUnit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationalUnitId"
                  Prelude..= organizationalUnitId
              )
          ]
      )

instance Prelude.ToPath DeleteOrganizationalUnit where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOrganizationalUnit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationalUnitResponse' smart constructor.
data DeleteOrganizationalUnitResponse = DeleteOrganizationalUnitResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
