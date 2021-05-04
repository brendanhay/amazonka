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
-- Module      : Network.AWS.WorkMail.AssociateDelegateToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the resource\'s set of delegates.
module Network.AWS.WorkMail.AssociateDelegateToResource
  ( -- * Creating a Request
    AssociateDelegateToResource (..),
    newAssociateDelegateToResource,

    -- * Request Lenses
    associateDelegateToResource_organizationId,
    associateDelegateToResource_resourceId,
    associateDelegateToResource_entityId,

    -- * Destructuring the Response
    AssociateDelegateToResourceResponse (..),
    newAssociateDelegateToResourceResponse,

    -- * Response Lenses
    associateDelegateToResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newAssociateDelegateToResource' smart constructor.
data AssociateDelegateToResource = AssociateDelegateToResource'
  { -- | The organization under which the resource exists.
    organizationId :: Prelude.Text,
    -- | The resource for which members (users or groups) are associated.
    resourceId :: Prelude.Text,
    -- | The member (user or group) to associate to the resource.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateDelegateToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'associateDelegateToResource_organizationId' - The organization under which the resource exists.
--
-- 'resourceId', 'associateDelegateToResource_resourceId' - The resource for which members (users or groups) are associated.
--
-- 'entityId', 'associateDelegateToResource_entityId' - The member (user or group) to associate to the resource.
newAssociateDelegateToResource ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  AssociateDelegateToResource
newAssociateDelegateToResource
  pOrganizationId_
  pResourceId_
  pEntityId_ =
    AssociateDelegateToResource'
      { organizationId =
          pOrganizationId_,
        resourceId = pResourceId_,
        entityId = pEntityId_
      }

-- | The organization under which the resource exists.
associateDelegateToResource_organizationId :: Lens.Lens' AssociateDelegateToResource Prelude.Text
associateDelegateToResource_organizationId = Lens.lens (\AssociateDelegateToResource' {organizationId} -> organizationId) (\s@AssociateDelegateToResource' {} a -> s {organizationId = a} :: AssociateDelegateToResource)

-- | The resource for which members (users or groups) are associated.
associateDelegateToResource_resourceId :: Lens.Lens' AssociateDelegateToResource Prelude.Text
associateDelegateToResource_resourceId = Lens.lens (\AssociateDelegateToResource' {resourceId} -> resourceId) (\s@AssociateDelegateToResource' {} a -> s {resourceId = a} :: AssociateDelegateToResource)

-- | The member (user or group) to associate to the resource.
associateDelegateToResource_entityId :: Lens.Lens' AssociateDelegateToResource Prelude.Text
associateDelegateToResource_entityId = Lens.lens (\AssociateDelegateToResource' {entityId} -> entityId) (\s@AssociateDelegateToResource' {} a -> s {entityId = a} :: AssociateDelegateToResource)

instance
  Prelude.AWSRequest
    AssociateDelegateToResource
  where
  type
    Rs AssociateDelegateToResource =
      AssociateDelegateToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDelegateToResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDelegateToResource

instance Prelude.NFData AssociateDelegateToResource

instance
  Prelude.ToHeaders
    AssociateDelegateToResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.AssociateDelegateToResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateDelegateToResource where
  toJSON AssociateDelegateToResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just ("EntityId" Prelude..= entityId)
          ]
      )

instance Prelude.ToPath AssociateDelegateToResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateDelegateToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDelegateToResourceResponse' smart constructor.
data AssociateDelegateToResourceResponse = AssociateDelegateToResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateDelegateToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDelegateToResourceResponse_httpStatus' - The response's http status code.
newAssociateDelegateToResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDelegateToResourceResponse
newAssociateDelegateToResourceResponse pHttpStatus_ =
  AssociateDelegateToResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDelegateToResourceResponse_httpStatus :: Lens.Lens' AssociateDelegateToResourceResponse Prelude.Int
associateDelegateToResourceResponse_httpStatus = Lens.lens (\AssociateDelegateToResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateDelegateToResourceResponse' {} a -> s {httpStatus = a} :: AssociateDelegateToResourceResponse)

instance
  Prelude.NFData
    AssociateDelegateToResourceResponse
