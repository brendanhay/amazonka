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
-- Module      : Network.AWS.Config.DeleteOrganizationConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization conformance pack and all of the
-- config rules and remediation actions from all member accounts in that
-- organization.
--
-- Only a master account or a delegated administrator account can delete an
-- organization conformance pack. When calling this API with a delegated
-- administrator, you must ensure AWS Organizations
-- @ListDelegatedAdministrator@ permissions are added.
--
-- AWS Config sets the state of a conformance pack to DELETE_IN_PROGRESS
-- until the deletion is complete. You cannot update a conformance pack
-- while it is in this state.
module Network.AWS.Config.DeleteOrganizationConformancePack
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOrganizationConformancePack' smart constructor.
data DeleteOrganizationConformancePack = DeleteOrganizationConformancePack'
  { -- | The name of organization conformance pack that you want to delete.
    organizationConformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteOrganizationConformancePack
  where
  type
    Rs DeleteOrganizationConformancePack =
      DeleteOrganizationConformancePackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteOrganizationConformancePackResponse'

instance
  Prelude.Hashable
    DeleteOrganizationConformancePack

instance
  Prelude.NFData
    DeleteOrganizationConformancePack

instance
  Prelude.ToHeaders
    DeleteOrganizationConformancePack
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteOrganizationConformancePack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteOrganizationConformancePack
  where
  toJSON DeleteOrganizationConformancePack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationConformancePackName"
                  Prelude..= organizationConformancePackName
              )
          ]
      )

instance
  Prelude.ToPath
    DeleteOrganizationConformancePack
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteOrganizationConformancePack
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationConformancePackResponse' smart constructor.
data DeleteOrganizationConformancePackResponse = DeleteOrganizationConformancePackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
