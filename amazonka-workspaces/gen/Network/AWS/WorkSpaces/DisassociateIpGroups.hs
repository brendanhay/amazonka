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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDisassociateIpGroups' smart constructor.
data DisassociateIpGroups = DisassociateIpGroups'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisassociateIpGroups
newDisassociateIpGroups pDirectoryId_ =
  DisassociateIpGroups'
    { directoryId = pDirectoryId_,
      groupIds = Prelude.mempty
    }

-- | The identifier of the directory.
disassociateIpGroups_directoryId :: Lens.Lens' DisassociateIpGroups Prelude.Text
disassociateIpGroups_directoryId = Lens.lens (\DisassociateIpGroups' {directoryId} -> directoryId) (\s@DisassociateIpGroups' {} a -> s {directoryId = a} :: DisassociateIpGroups)

-- | The identifiers of one or more IP access control groups.
disassociateIpGroups_groupIds :: Lens.Lens' DisassociateIpGroups [Prelude.Text]
disassociateIpGroups_groupIds = Lens.lens (\DisassociateIpGroups' {groupIds} -> groupIds) (\s@DisassociateIpGroups' {} a -> s {groupIds = a} :: DisassociateIpGroups) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DisassociateIpGroups where
  type
    Rs DisassociateIpGroups =
      DisassociateIpGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateIpGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateIpGroups

instance Prelude.NFData DisassociateIpGroups

instance Prelude.ToHeaders DisassociateIpGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DisassociateIpGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateIpGroups where
  toJSON DisassociateIpGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("GroupIds" Prelude..= groupIds)
          ]
      )

instance Prelude.ToPath DisassociateIpGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateIpGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateIpGroupsResponse' smart constructor.
data DisassociateIpGroupsResponse = DisassociateIpGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateIpGroupsResponse
newDisassociateIpGroupsResponse pHttpStatus_ =
  DisassociateIpGroupsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateIpGroupsResponse_httpStatus :: Lens.Lens' DisassociateIpGroupsResponse Prelude.Int
disassociateIpGroupsResponse_httpStatus = Lens.lens (\DisassociateIpGroupsResponse' {httpStatus} -> httpStatus) (\s@DisassociateIpGroupsResponse' {} a -> s {httpStatus = a} :: DisassociateIpGroupsResponse)

instance Prelude.NFData DisassociateIpGroupsResponse
