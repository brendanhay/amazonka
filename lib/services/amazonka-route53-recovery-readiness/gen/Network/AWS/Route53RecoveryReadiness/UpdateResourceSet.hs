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
-- Module      : Network.AWS.Route53RecoveryReadiness.UpdateResourceSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Resource Set.
module Network.AWS.Route53RecoveryReadiness.UpdateResourceSet
  ( -- * Creating a Request
    UpdateResourceSet (..),
    newUpdateResourceSet,

    -- * Request Lenses
    updateResourceSet_resourceSetName,
    updateResourceSet_resourceSetType,
    updateResourceSet_resources,

    -- * Destructuring the Response
    UpdateResourceSetResponse (..),
    newUpdateResourceSetResponse,

    -- * Response Lenses
    updateResourceSetResponse_resourceSetName,
    updateResourceSetResponse_resourceSetType,
    updateResourceSetResponse_resources,
    updateResourceSetResponse_resourceSetArn,
    updateResourceSetResponse_tags,
    updateResourceSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | configuration for the desired
--
-- /See:/ 'newUpdateResourceSet' smart constructor.
data UpdateResourceSet = UpdateResourceSet'
  { -- | The ResourceSet to update
    resourceSetName :: Prelude.Text,
    -- | AWS Resource Type of the resources in the ResourceSet
    resourceSetType :: Prelude.Text,
    -- | A list of Resource objects
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'updateResourceSet_resourceSetName' - The ResourceSet to update
--
-- 'resourceSetType', 'updateResourceSet_resourceSetType' - AWS Resource Type of the resources in the ResourceSet
--
-- 'resources', 'updateResourceSet_resources' - A list of Resource objects
newUpdateResourceSet ::
  -- | 'resourceSetName'
  Prelude.Text ->
  -- | 'resourceSetType'
  Prelude.Text ->
  UpdateResourceSet
newUpdateResourceSet
  pResourceSetName_
  pResourceSetType_ =
    UpdateResourceSet'
      { resourceSetName =
          pResourceSetName_,
        resourceSetType = pResourceSetType_,
        resources = Prelude.mempty
      }

-- | The ResourceSet to update
updateResourceSet_resourceSetName :: Lens.Lens' UpdateResourceSet Prelude.Text
updateResourceSet_resourceSetName = Lens.lens (\UpdateResourceSet' {resourceSetName} -> resourceSetName) (\s@UpdateResourceSet' {} a -> s {resourceSetName = a} :: UpdateResourceSet)

-- | AWS Resource Type of the resources in the ResourceSet
updateResourceSet_resourceSetType :: Lens.Lens' UpdateResourceSet Prelude.Text
updateResourceSet_resourceSetType = Lens.lens (\UpdateResourceSet' {resourceSetType} -> resourceSetType) (\s@UpdateResourceSet' {} a -> s {resourceSetType = a} :: UpdateResourceSet)

-- | A list of Resource objects
updateResourceSet_resources :: Lens.Lens' UpdateResourceSet [Resource]
updateResourceSet_resources = Lens.lens (\UpdateResourceSet' {resources} -> resources) (\s@UpdateResourceSet' {} a -> s {resources = a} :: UpdateResourceSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateResourceSet where
  type
    AWSResponse UpdateResourceSet =
      UpdateResourceSetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceSetResponse'
            Prelude.<$> (x Core..?> "resourceSetName")
            Prelude.<*> (x Core..?> "resourceSetType")
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceSetArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceSet

instance Prelude.NFData UpdateResourceSet

instance Core.ToHeaders UpdateResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResourceSet where
  toJSON UpdateResourceSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceSetType" Core..= resourceSetType),
            Prelude.Just ("resources" Core..= resources)
          ]
      )

instance Core.ToPath UpdateResourceSet where
  toPath UpdateResourceSet' {..} =
    Prelude.mconcat
      ["/resourcesets/", Core.toBS resourceSetName]

instance Core.ToQuery UpdateResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceSetResponse' smart constructor.
data UpdateResourceSetResponse = UpdateResourceSetResponse'
  { -- | The name of the ResourceSet
    resourceSetName :: Prelude.Maybe Prelude.Text,
    -- | AWS Resource Type of the resources in the ResourceSet
    resourceSetType :: Prelude.Maybe Prelude.Text,
    -- | A list of Resource objects
    resources :: Prelude.Maybe [Resource],
    -- | The arn for the ResourceSet
    resourceSetArn :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'updateResourceSetResponse_resourceSetName' - The name of the ResourceSet
--
-- 'resourceSetType', 'updateResourceSetResponse_resourceSetType' - AWS Resource Type of the resources in the ResourceSet
--
-- 'resources', 'updateResourceSetResponse_resources' - A list of Resource objects
--
-- 'resourceSetArn', 'updateResourceSetResponse_resourceSetArn' - The arn for the ResourceSet
--
-- 'tags', 'updateResourceSetResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'updateResourceSetResponse_httpStatus' - The response's http status code.
newUpdateResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceSetResponse
newUpdateResourceSetResponse pHttpStatus_ =
  UpdateResourceSetResponse'
    { resourceSetName =
        Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resources = Prelude.Nothing,
      resourceSetArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the ResourceSet
updateResourceSetResponse_resourceSetName :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetName = Lens.lens (\UpdateResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetName = a} :: UpdateResourceSetResponse)

-- | AWS Resource Type of the resources in the ResourceSet
updateResourceSetResponse_resourceSetType :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetType = Lens.lens (\UpdateResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetType = a} :: UpdateResourceSetResponse)

-- | A list of Resource objects
updateResourceSetResponse_resources :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe [Resource])
updateResourceSetResponse_resources = Lens.lens (\UpdateResourceSetResponse' {resources} -> resources) (\s@UpdateResourceSetResponse' {} a -> s {resources = a} :: UpdateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The arn for the ResourceSet
updateResourceSetResponse_resourceSetArn :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetArn = Lens.lens (\UpdateResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetArn = a} :: UpdateResourceSetResponse)

-- | Undocumented member.
updateResourceSetResponse_tags :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateResourceSetResponse_tags = Lens.lens (\UpdateResourceSetResponse' {tags} -> tags) (\s@UpdateResourceSetResponse' {} a -> s {tags = a} :: UpdateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateResourceSetResponse_httpStatus :: Lens.Lens' UpdateResourceSetResponse Prelude.Int
updateResourceSetResponse_httpStatus = Lens.lens (\UpdateResourceSetResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceSetResponse' {} a -> s {httpStatus = a} :: UpdateResourceSetResponse)

instance Prelude.NFData UpdateResourceSetResponse
