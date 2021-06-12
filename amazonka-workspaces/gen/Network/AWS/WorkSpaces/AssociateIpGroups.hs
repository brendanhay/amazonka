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
-- Module      : Network.AWS.WorkSpaces.AssociateIpGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified IP access control group with the specified
-- directory.
module Network.AWS.WorkSpaces.AssociateIpGroups
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newAssociateIpGroups' smart constructor.
data AssociateIpGroups = AssociateIpGroups'
  { -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AssociateIpGroups
newAssociateIpGroups pDirectoryId_ =
  AssociateIpGroups'
    { directoryId = pDirectoryId_,
      groupIds = Core.mempty
    }

-- | The identifier of the directory.
associateIpGroups_directoryId :: Lens.Lens' AssociateIpGroups Core.Text
associateIpGroups_directoryId = Lens.lens (\AssociateIpGroups' {directoryId} -> directoryId) (\s@AssociateIpGroups' {} a -> s {directoryId = a} :: AssociateIpGroups)

-- | The identifiers of one or more IP access control groups.
associateIpGroups_groupIds :: Lens.Lens' AssociateIpGroups [Core.Text]
associateIpGroups_groupIds = Lens.lens (\AssociateIpGroups' {groupIds} -> groupIds) (\s@AssociateIpGroups' {} a -> s {groupIds = a} :: AssociateIpGroups) Core.. Lens._Coerce

instance Core.AWSRequest AssociateIpGroups where
  type
    AWSResponse AssociateIpGroups =
      AssociateIpGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateIpGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateIpGroups

instance Core.NFData AssociateIpGroups

instance Core.ToHeaders AssociateIpGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.AssociateIpGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateIpGroups where
  toJSON AssociateIpGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("GroupIds" Core..= groupIds)
          ]
      )

instance Core.ToPath AssociateIpGroups where
  toPath = Core.const "/"

instance Core.ToQuery AssociateIpGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateIpGroupsResponse' smart constructor.
data AssociateIpGroupsResponse = AssociateIpGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociateIpGroupsResponse
newAssociateIpGroupsResponse pHttpStatus_ =
  AssociateIpGroupsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateIpGroupsResponse_httpStatus :: Lens.Lens' AssociateIpGroupsResponse Core.Int
associateIpGroupsResponse_httpStatus = Lens.lens (\AssociateIpGroupsResponse' {httpStatus} -> httpStatus) (\s@AssociateIpGroupsResponse' {} a -> s {httpStatus = a} :: AssociateIpGroupsResponse)

instance Core.NFData AssociateIpGroupsResponse
