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
-- Module      : Amazonka.WorkSpaces.AssociateIpGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified IP access control group with the specified
-- directory.
module Amazonka.WorkSpaces.AssociateIpGroups
  ( -- * Creating a Request
    AssociateIpGroups (..),
    newAssociateIpGroups,

    -- * Request Lenses
    associateIpGroups_directoryId,
    associateIpGroups_groupIds,

    -- * Destructuring the Response
    AssociateIpGroupsResponse (..),
    newAssociateIpGroupsResponse,

    -- * Response Lenses
    associateIpGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newAssociateIpGroups' smart constructor.
data AssociateIpGroups = AssociateIpGroups'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'associateIpGroups_directoryId' - The identifier of the directory.
--
-- 'groupIds', 'associateIpGroups_groupIds' - The identifiers of one or more IP access control groups.
newAssociateIpGroups ::
  -- | 'directoryId'
  Prelude.Text ->
  AssociateIpGroups
newAssociateIpGroups pDirectoryId_ =
  AssociateIpGroups'
    { directoryId = pDirectoryId_,
      groupIds = Prelude.mempty
    }

-- | The identifier of the directory.
associateIpGroups_directoryId :: Lens.Lens' AssociateIpGroups Prelude.Text
associateIpGroups_directoryId = Lens.lens (\AssociateIpGroups' {directoryId} -> directoryId) (\s@AssociateIpGroups' {} a -> s {directoryId = a} :: AssociateIpGroups)

-- | The identifiers of one or more IP access control groups.
associateIpGroups_groupIds :: Lens.Lens' AssociateIpGroups [Prelude.Text]
associateIpGroups_groupIds = Lens.lens (\AssociateIpGroups' {groupIds} -> groupIds) (\s@AssociateIpGroups' {} a -> s {groupIds = a} :: AssociateIpGroups) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateIpGroups where
  type
    AWSResponse AssociateIpGroups =
      AssociateIpGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateIpGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateIpGroups where
  hashWithSalt _salt AssociateIpGroups' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` groupIds

instance Prelude.NFData AssociateIpGroups where
  rnf AssociateIpGroups' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf groupIds

instance Data.ToHeaders AssociateIpGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.AssociateIpGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateIpGroups where
  toJSON AssociateIpGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("GroupIds" Data..= groupIds)
          ]
      )

instance Data.ToPath AssociateIpGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateIpGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateIpGroupsResponse' smart constructor.
data AssociateIpGroupsResponse = AssociateIpGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateIpGroupsResponse_httpStatus' - The response's http status code.
newAssociateIpGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateIpGroupsResponse
newAssociateIpGroupsResponse pHttpStatus_ =
  AssociateIpGroupsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateIpGroupsResponse_httpStatus :: Lens.Lens' AssociateIpGroupsResponse Prelude.Int
associateIpGroupsResponse_httpStatus = Lens.lens (\AssociateIpGroupsResponse' {httpStatus} -> httpStatus) (\s@AssociateIpGroupsResponse' {} a -> s {httpStatus = a} :: AssociateIpGroupsResponse)

instance Prelude.NFData AssociateIpGroupsResponse where
  rnf AssociateIpGroupsResponse' {..} =
    Prelude.rnf httpStatus
