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
-- Module      : Network.AWS.WorkMail.GetDefaultRetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the default retention policy details for the specified
-- organization.
module Network.AWS.WorkMail.GetDefaultRetentionPolicy
  ( -- * Creating a Request
    GetDefaultRetentionPolicy (..),
    newGetDefaultRetentionPolicy,

    -- * Request Lenses
    getDefaultRetentionPolicy_organizationId,

    -- * Destructuring the Response
    GetDefaultRetentionPolicyResponse (..),
    newGetDefaultRetentionPolicyResponse,

    -- * Response Lenses
    getDefaultRetentionPolicyResponse_id,
    getDefaultRetentionPolicyResponse_folderConfigurations,
    getDefaultRetentionPolicyResponse_name,
    getDefaultRetentionPolicyResponse_description,
    getDefaultRetentionPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newGetDefaultRetentionPolicy' smart constructor.
data GetDefaultRetentionPolicy = GetDefaultRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getDefaultRetentionPolicy_organizationId' - The organization ID.
newGetDefaultRetentionPolicy ::
  -- | 'organizationId'
  Prelude.Text ->
  GetDefaultRetentionPolicy
newGetDefaultRetentionPolicy pOrganizationId_ =
  GetDefaultRetentionPolicy'
    { organizationId =
        pOrganizationId_
    }

-- | The organization ID.
getDefaultRetentionPolicy_organizationId :: Lens.Lens' GetDefaultRetentionPolicy Prelude.Text
getDefaultRetentionPolicy_organizationId = Lens.lens (\GetDefaultRetentionPolicy' {organizationId} -> organizationId) (\s@GetDefaultRetentionPolicy' {} a -> s {organizationId = a} :: GetDefaultRetentionPolicy)

instance Core.AWSRequest GetDefaultRetentionPolicy where
  type
    AWSResponse GetDefaultRetentionPolicy =
      GetDefaultRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultRetentionPolicyResponse'
            Prelude.<$> (x Core..?> "Id")
            Prelude.<*> ( x Core..?> "FolderConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDefaultRetentionPolicy

instance Prelude.NFData GetDefaultRetentionPolicy

instance Core.ToHeaders GetDefaultRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.GetDefaultRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDefaultRetentionPolicy where
  toJSON GetDefaultRetentionPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath GetDefaultRetentionPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDefaultRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDefaultRetentionPolicyResponse' smart constructor.
data GetDefaultRetentionPolicyResponse = GetDefaultRetentionPolicyResponse'
  { -- | The retention policy ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The retention policy folder configurations.
    folderConfigurations :: Prelude.Maybe [FolderConfiguration],
    -- | The retention policy name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The retention policy description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getDefaultRetentionPolicyResponse_id' - The retention policy ID.
--
-- 'folderConfigurations', 'getDefaultRetentionPolicyResponse_folderConfigurations' - The retention policy folder configurations.
--
-- 'name', 'getDefaultRetentionPolicyResponse_name' - The retention policy name.
--
-- 'description', 'getDefaultRetentionPolicyResponse_description' - The retention policy description.
--
-- 'httpStatus', 'getDefaultRetentionPolicyResponse_httpStatus' - The response's http status code.
newGetDefaultRetentionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDefaultRetentionPolicyResponse
newGetDefaultRetentionPolicyResponse pHttpStatus_ =
  GetDefaultRetentionPolicyResponse'
    { id =
        Prelude.Nothing,
      folderConfigurations = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The retention policy ID.
getDefaultRetentionPolicyResponse_id :: Lens.Lens' GetDefaultRetentionPolicyResponse (Prelude.Maybe Prelude.Text)
getDefaultRetentionPolicyResponse_id = Lens.lens (\GetDefaultRetentionPolicyResponse' {id} -> id) (\s@GetDefaultRetentionPolicyResponse' {} a -> s {id = a} :: GetDefaultRetentionPolicyResponse)

-- | The retention policy folder configurations.
getDefaultRetentionPolicyResponse_folderConfigurations :: Lens.Lens' GetDefaultRetentionPolicyResponse (Prelude.Maybe [FolderConfiguration])
getDefaultRetentionPolicyResponse_folderConfigurations = Lens.lens (\GetDefaultRetentionPolicyResponse' {folderConfigurations} -> folderConfigurations) (\s@GetDefaultRetentionPolicyResponse' {} a -> s {folderConfigurations = a} :: GetDefaultRetentionPolicyResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The retention policy name.
getDefaultRetentionPolicyResponse_name :: Lens.Lens' GetDefaultRetentionPolicyResponse (Prelude.Maybe Prelude.Text)
getDefaultRetentionPolicyResponse_name = Lens.lens (\GetDefaultRetentionPolicyResponse' {name} -> name) (\s@GetDefaultRetentionPolicyResponse' {} a -> s {name = a} :: GetDefaultRetentionPolicyResponse)

-- | The retention policy description.
getDefaultRetentionPolicyResponse_description :: Lens.Lens' GetDefaultRetentionPolicyResponse (Prelude.Maybe Prelude.Text)
getDefaultRetentionPolicyResponse_description = Lens.lens (\GetDefaultRetentionPolicyResponse' {description} -> description) (\s@GetDefaultRetentionPolicyResponse' {} a -> s {description = a} :: GetDefaultRetentionPolicyResponse)

-- | The response's http status code.
getDefaultRetentionPolicyResponse_httpStatus :: Lens.Lens' GetDefaultRetentionPolicyResponse Prelude.Int
getDefaultRetentionPolicyResponse_httpStatus = Lens.lens (\GetDefaultRetentionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetDefaultRetentionPolicyResponse' {} a -> s {httpStatus = a} :: GetDefaultRetentionPolicyResponse)

instance
  Prelude.NFData
    GetDefaultRetentionPolicyResponse
