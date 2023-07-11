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
-- Module      : Amazonka.WorkMail.DisassociateDelegateFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from the resource\'s set of delegates.
module Amazonka.WorkMail.DisassociateDelegateFromResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DisassociateDelegateFromResource
  where
  type
    AWSResponse DisassociateDelegateFromResource =
      DisassociateDelegateFromResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDelegateFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateDelegateFromResource
  where
  hashWithSalt
    _salt
    DisassociateDelegateFromResource' {..} =
      _salt
        `Prelude.hashWithSalt` organizationId
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` entityId

instance
  Prelude.NFData
    DisassociateDelegateFromResource
  where
  rnf DisassociateDelegateFromResource' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf entityId

instance
  Data.ToHeaders
    DisassociateDelegateFromResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DisassociateDelegateFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDelegateFromResource where
  toJSON DisassociateDelegateFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("EntityId" Data..= entityId)
          ]
      )

instance Data.ToPath DisassociateDelegateFromResource where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateDelegateFromResource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDelegateFromResourceResponse' smart constructor.
data DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateDelegateFromResourceResponse' {..} =
    Prelude.rnf httpStatus
