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
-- Module      : Network.AWS.WorkMail.PutRetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a retention policy to the specified organization.
module Network.AWS.WorkMail.PutRetentionPolicy
  ( -- * Creating a Request
    PutRetentionPolicy (..),
    newPutRetentionPolicy,

    -- * Request Lenses
    putRetentionPolicy_id,
    putRetentionPolicy_description,
    putRetentionPolicy_organizationId,
    putRetentionPolicy_name,
    putRetentionPolicy_folderConfigurations,

    -- * Destructuring the Response
    PutRetentionPolicyResponse (..),
    newPutRetentionPolicyResponse,

    -- * Response Lenses
    putRetentionPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The retention policy ID.
    id :: Core.Maybe Core.Text,
    -- | The retention policy description.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The organization ID.
    organizationId :: Core.Text,
    -- | The retention policy name.
    name :: Core.Text,
    -- | The retention policy folder configurations.
    folderConfigurations :: [FolderConfiguration]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'putRetentionPolicy_id' - The retention policy ID.
--
-- 'description', 'putRetentionPolicy_description' - The retention policy description.
--
-- 'organizationId', 'putRetentionPolicy_organizationId' - The organization ID.
--
-- 'name', 'putRetentionPolicy_name' - The retention policy name.
--
-- 'folderConfigurations', 'putRetentionPolicy_folderConfigurations' - The retention policy folder configurations.
newPutRetentionPolicy ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  PutRetentionPolicy
newPutRetentionPolicy pOrganizationId_ pName_ =
  PutRetentionPolicy'
    { id = Core.Nothing,
      description = Core.Nothing,
      organizationId = pOrganizationId_,
      name = pName_,
      folderConfigurations = Core.mempty
    }

-- | The retention policy ID.
putRetentionPolicy_id :: Lens.Lens' PutRetentionPolicy (Core.Maybe Core.Text)
putRetentionPolicy_id = Lens.lens (\PutRetentionPolicy' {id} -> id) (\s@PutRetentionPolicy' {} a -> s {id = a} :: PutRetentionPolicy)

-- | The retention policy description.
putRetentionPolicy_description :: Lens.Lens' PutRetentionPolicy (Core.Maybe Core.Text)
putRetentionPolicy_description = Lens.lens (\PutRetentionPolicy' {description} -> description) (\s@PutRetentionPolicy' {} a -> s {description = a} :: PutRetentionPolicy) Core.. Lens.mapping Core._Sensitive

-- | The organization ID.
putRetentionPolicy_organizationId :: Lens.Lens' PutRetentionPolicy Core.Text
putRetentionPolicy_organizationId = Lens.lens (\PutRetentionPolicy' {organizationId} -> organizationId) (\s@PutRetentionPolicy' {} a -> s {organizationId = a} :: PutRetentionPolicy)

-- | The retention policy name.
putRetentionPolicy_name :: Lens.Lens' PutRetentionPolicy Core.Text
putRetentionPolicy_name = Lens.lens (\PutRetentionPolicy' {name} -> name) (\s@PutRetentionPolicy' {} a -> s {name = a} :: PutRetentionPolicy)

-- | The retention policy folder configurations.
putRetentionPolicy_folderConfigurations :: Lens.Lens' PutRetentionPolicy [FolderConfiguration]
putRetentionPolicy_folderConfigurations = Lens.lens (\PutRetentionPolicy' {folderConfigurations} -> folderConfigurations) (\s@PutRetentionPolicy' {} a -> s {folderConfigurations = a} :: PutRetentionPolicy) Core.. Lens._Coerce

instance Core.AWSRequest PutRetentionPolicy where
  type
    AWSResponse PutRetentionPolicy =
      PutRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRetentionPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRetentionPolicy

instance Core.NFData PutRetentionPolicy

instance Core.ToHeaders PutRetentionPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.PutRetentionPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Id" Core..=) Core.<$> id,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name),
            Core.Just
              ( "FolderConfigurations"
                  Core..= folderConfigurations
              )
          ]
      )

instance Core.ToPath PutRetentionPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutRetentionPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRetentionPolicyResponse_httpStatus' - The response's http status code.
newPutRetentionPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRetentionPolicyResponse
newPutRetentionPolicyResponse pHttpStatus_ =
  PutRetentionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRetentionPolicyResponse_httpStatus :: Lens.Lens' PutRetentionPolicyResponse Core.Int
putRetentionPolicyResponse_httpStatus = Lens.lens (\PutRetentionPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRetentionPolicyResponse' {} a -> s {httpStatus = a} :: PutRetentionPolicyResponse)

instance Core.NFData PutRetentionPolicyResponse
