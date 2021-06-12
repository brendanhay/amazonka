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
-- Module      : Network.AWS.WorkSpaces.DisassociateIpGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified IP access control group from the specified
-- directory.
module Network.AWS.WorkSpaces.DisassociateIpGroups
  ( -- * Creating a Request
    DisassociateIpGroups (..),
    newDisassociateIpGroups,

    -- * Request Lenses
    disassociateIpGroups_directoryId,
    disassociateIpGroups_groupIds,

    -- * Destructuring the Response
    DisassociateIpGroupsResponse (..),
    newDisassociateIpGroupsResponse,

    -- * Response Lenses
    disassociateIpGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDisassociateIpGroups' smart constructor.
data DisassociateIpGroups = DisassociateIpGroups'
  { -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateIpGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'disassociateIpGroups_directoryId' - The identifier of the directory.
--
-- 'groupIds', 'disassociateIpGroups_groupIds' - The identifiers of one or more IP access control groups.
newDisassociateIpGroups ::
  -- | 'directoryId'
  Core.Text ->
  DisassociateIpGroups
newDisassociateIpGroups pDirectoryId_ =
  DisassociateIpGroups'
    { directoryId = pDirectoryId_,
      groupIds = Core.mempty
    }

-- | The identifier of the directory.
disassociateIpGroups_directoryId :: Lens.Lens' DisassociateIpGroups Core.Text
disassociateIpGroups_directoryId = Lens.lens (\DisassociateIpGroups' {directoryId} -> directoryId) (\s@DisassociateIpGroups' {} a -> s {directoryId = a} :: DisassociateIpGroups)

-- | The identifiers of one or more IP access control groups.
disassociateIpGroups_groupIds :: Lens.Lens' DisassociateIpGroups [Core.Text]
disassociateIpGroups_groupIds = Lens.lens (\DisassociateIpGroups' {groupIds} -> groupIds) (\s@DisassociateIpGroups' {} a -> s {groupIds = a} :: DisassociateIpGroups) Core.. Lens._Coerce

instance Core.AWSRequest DisassociateIpGroups where
  type
    AWSResponse DisassociateIpGroups =
      DisassociateIpGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateIpGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateIpGroups

instance Core.NFData DisassociateIpGroups

instance Core.ToHeaders DisassociateIpGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DisassociateIpGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateIpGroups where
  toJSON DisassociateIpGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("GroupIds" Core..= groupIds)
          ]
      )

instance Core.ToPath DisassociateIpGroups where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateIpGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateIpGroupsResponse' smart constructor.
data DisassociateIpGroupsResponse = DisassociateIpGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateIpGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateIpGroupsResponse_httpStatus' - The response's http status code.
newDisassociateIpGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateIpGroupsResponse
newDisassociateIpGroupsResponse pHttpStatus_ =
  DisassociateIpGroupsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateIpGroupsResponse_httpStatus :: Lens.Lens' DisassociateIpGroupsResponse Core.Int
disassociateIpGroupsResponse_httpStatus = Lens.lens (\DisassociateIpGroupsResponse' {httpStatus} -> httpStatus) (\s@DisassociateIpGroupsResponse' {} a -> s {httpStatus = a} :: DisassociateIpGroupsResponse)

instance Core.NFData DisassociateIpGroupsResponse
