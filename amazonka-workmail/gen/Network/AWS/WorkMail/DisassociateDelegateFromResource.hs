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
-- Module      : Network.AWS.WorkMail.DisassociateDelegateFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from the resource\'s set of delegates.
module Network.AWS.WorkMail.DisassociateDelegateFromResource
  ( -- * Creating a Request
    DisassociateDelegateFromResource (..),
    newDisassociateDelegateFromResource,

    -- * Request Lenses
    disassociateDelegateFromResource_organizationId,
    disassociateDelegateFromResource_resourceId,
    disassociateDelegateFromResource_entityId,

    -- * Destructuring the Response
    DisassociateDelegateFromResourceResponse (..),
    newDisassociateDelegateFromResourceResponse,

    -- * Response Lenses
    disassociateDelegateFromResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDisassociateDelegateFromResource' smart constructor.
data DisassociateDelegateFromResource = DisassociateDelegateFromResource'
  { -- | The identifier for the organization under which the resource exists.
    organizationId :: Prelude.Text,
    -- | The identifier of the resource from which delegates\' set members are
    -- removed.
    resourceId :: Prelude.Text,
    -- | The identifier for the member (user, group) to be removed from the
    -- resource\'s delegates.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDelegateFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'disassociateDelegateFromResource_organizationId' - The identifier for the organization under which the resource exists.
--
-- 'resourceId', 'disassociateDelegateFromResource_resourceId' - The identifier of the resource from which delegates\' set members are
-- removed.
--
-- 'entityId', 'disassociateDelegateFromResource_entityId' - The identifier for the member (user, group) to be removed from the
-- resource\'s delegates.
newDisassociateDelegateFromResource ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  DisassociateDelegateFromResource
newDisassociateDelegateFromResource
  pOrganizationId_
  pResourceId_
  pEntityId_ =
    DisassociateDelegateFromResource'
      { organizationId =
          pOrganizationId_,
        resourceId = pResourceId_,
        entityId = pEntityId_
      }

-- | The identifier for the organization under which the resource exists.
disassociateDelegateFromResource_organizationId :: Lens.Lens' DisassociateDelegateFromResource Prelude.Text
disassociateDelegateFromResource_organizationId = Lens.lens (\DisassociateDelegateFromResource' {organizationId} -> organizationId) (\s@DisassociateDelegateFromResource' {} a -> s {organizationId = a} :: DisassociateDelegateFromResource)

-- | The identifier of the resource from which delegates\' set members are
-- removed.
disassociateDelegateFromResource_resourceId :: Lens.Lens' DisassociateDelegateFromResource Prelude.Text
disassociateDelegateFromResource_resourceId = Lens.lens (\DisassociateDelegateFromResource' {resourceId} -> resourceId) (\s@DisassociateDelegateFromResource' {} a -> s {resourceId = a} :: DisassociateDelegateFromResource)

-- | The identifier for the member (user, group) to be removed from the
-- resource\'s delegates.
disassociateDelegateFromResource_entityId :: Lens.Lens' DisassociateDelegateFromResource Prelude.Text
disassociateDelegateFromResource_entityId = Lens.lens (\DisassociateDelegateFromResource' {entityId} -> entityId) (\s@DisassociateDelegateFromResource' {} a -> s {entityId = a} :: DisassociateDelegateFromResource)

instance
  Prelude.AWSRequest
    DisassociateDelegateFromResource
  where
  type
    Rs DisassociateDelegateFromResource =
      DisassociateDelegateFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDelegateFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateDelegateFromResource

instance
  Prelude.NFData
    DisassociateDelegateFromResource

instance
  Prelude.ToHeaders
    DisassociateDelegateFromResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DisassociateDelegateFromResource" ::
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
    DisassociateDelegateFromResource
  where
  toJSON DisassociateDelegateFromResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just ("EntityId" Prelude..= entityId)
          ]
      )

instance
  Prelude.ToPath
    DisassociateDelegateFromResource
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateDelegateFromResource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDelegateFromResourceResponse' smart constructor.
data DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDelegateFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDelegateFromResourceResponse_httpStatus' - The response's http status code.
newDisassociateDelegateFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDelegateFromResourceResponse
newDisassociateDelegateFromResourceResponse
  pHttpStatus_ =
    DisassociateDelegateFromResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateDelegateFromResourceResponse_httpStatus :: Lens.Lens' DisassociateDelegateFromResourceResponse Prelude.Int
disassociateDelegateFromResourceResponse_httpStatus = Lens.lens (\DisassociateDelegateFromResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateDelegateFromResourceResponse' {} a -> s {httpStatus = a} :: DisassociateDelegateFromResourceResponse)

instance
  Prelude.NFData
    DisassociateDelegateFromResourceResponse
