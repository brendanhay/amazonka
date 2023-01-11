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
-- Module      : Amazonka.Config.DeleteOrganizationConformancePack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization conformance pack and all of the
-- Config rules and remediation actions from all member accounts in that
-- organization.
--
-- Only a management account or a delegated administrator account can
-- delete an organization conformance pack. When calling this API with a
-- delegated administrator, you must ensure Organizations
-- @ListDelegatedAdministrator@ permissions are added.
--
-- Config sets the state of a conformance pack to DELETE_IN_PROGRESS until
-- the deletion is complete. You cannot update a conformance pack while it
-- is in this state.
module Amazonka.Config.DeleteOrganizationConformancePack
  ( -- * Creating a Request
    DeleteOrganizationConformancePack (..),
    newDeleteOrganizationConformancePack,

    -- * Request Lenses
    deleteOrganizationConformancePack_organizationConformancePackName,

    -- * Destructuring the Response
    DeleteOrganizationConformancePackResponse (..),
    newDeleteOrganizationConformancePackResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOrganizationConformancePack' smart constructor.
data DeleteOrganizationConformancePack = DeleteOrganizationConformancePack'
  { -- | The name of organization conformance pack that you want to delete.
    organizationConformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConformancePackName', 'deleteOrganizationConformancePack_organizationConformancePackName' - The name of organization conformance pack that you want to delete.
newDeleteOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Prelude.Text ->
  DeleteOrganizationConformancePack
newDeleteOrganizationConformancePack
  pOrganizationConformancePackName_ =
    DeleteOrganizationConformancePack'
      { organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | The name of organization conformance pack that you want to delete.
deleteOrganizationConformancePack_organizationConformancePackName :: Lens.Lens' DeleteOrganizationConformancePack Prelude.Text
deleteOrganizationConformancePack_organizationConformancePackName = Lens.lens (\DeleteOrganizationConformancePack' {organizationConformancePackName} -> organizationConformancePackName) (\s@DeleteOrganizationConformancePack' {} a -> s {organizationConformancePackName = a} :: DeleteOrganizationConformancePack)

instance
  Core.AWSRequest
    DeleteOrganizationConformancePack
  where
  type
    AWSResponse DeleteOrganizationConformancePack =
      DeleteOrganizationConformancePackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteOrganizationConformancePackResponse'

instance
  Prelude.Hashable
    DeleteOrganizationConformancePack
  where
  hashWithSalt
    _salt
    DeleteOrganizationConformancePack' {..} =
      _salt
        `Prelude.hashWithSalt` organizationConformancePackName

instance
  Prelude.NFData
    DeleteOrganizationConformancePack
  where
  rnf DeleteOrganizationConformancePack' {..} =
    Prelude.rnf organizationConformancePackName

instance
  Data.ToHeaders
    DeleteOrganizationConformancePack
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteOrganizationConformancePack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteOrganizationConformancePack
  where
  toJSON DeleteOrganizationConformancePack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationConformancePackName"
                  Data..= organizationConformancePackName
              )
          ]
      )

instance
  Data.ToPath
    DeleteOrganizationConformancePack
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteOrganizationConformancePack
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationConformancePackResponse' smart constructor.
data DeleteOrganizationConformancePackResponse = DeleteOrganizationConformancePackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOrganizationConformancePackResponse ::
  DeleteOrganizationConformancePackResponse
newDeleteOrganizationConformancePackResponse =
  DeleteOrganizationConformancePackResponse'

instance
  Prelude.NFData
    DeleteOrganizationConformancePackResponse
  where
  rnf _ = ()
