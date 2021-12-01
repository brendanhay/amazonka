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
-- Module      : Amazonka.Route53RecoveryReadiness.CreateResourceSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Resource Set.
module Amazonka.Route53RecoveryReadiness.CreateResourceSet
  ( -- * Creating a Request
    CreateResourceSet (..),
    newCreateResourceSet,

    -- * Request Lenses
    createResourceSet_tags,
    createResourceSet_resourceSetType,
    createResourceSet_resourceSetName,
    createResourceSet_resources,

    -- * Destructuring the Response
    CreateResourceSetResponse (..),
    newCreateResourceSetResponse,

    -- * Response Lenses
    createResourceSetResponse_resourceSetName,
    createResourceSetResponse_resourceSetType,
    createResourceSetResponse_resources,
    createResourceSetResponse_resourceSetArn,
    createResourceSetResponse_tags,
    createResourceSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | The ResourceSet to create
--
-- /See:/ 'newCreateResourceSet' smart constructor.
data CreateResourceSet = CreateResourceSet'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | AWS Resource type of the resources in the ResourceSet
    resourceSetType :: Prelude.Text,
    -- | The name of the ResourceSet to create
    resourceSetName :: Prelude.Text,
    -- | A list of Resource objects
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createResourceSet_tags' - Undocumented member.
--
-- 'resourceSetType', 'createResourceSet_resourceSetType' - AWS Resource type of the resources in the ResourceSet
--
-- 'resourceSetName', 'createResourceSet_resourceSetName' - The name of the ResourceSet to create
--
-- 'resources', 'createResourceSet_resources' - A list of Resource objects
newCreateResourceSet ::
  -- | 'resourceSetType'
  Prelude.Text ->
  -- | 'resourceSetName'
  Prelude.Text ->
  CreateResourceSet
newCreateResourceSet
  pResourceSetType_
  pResourceSetName_ =
    CreateResourceSet'
      { tags = Prelude.Nothing,
        resourceSetType = pResourceSetType_,
        resourceSetName = pResourceSetName_,
        resources = Prelude.mempty
      }

-- | Undocumented member.
createResourceSet_tags :: Lens.Lens' CreateResourceSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResourceSet_tags = Lens.lens (\CreateResourceSet' {tags} -> tags) (\s@CreateResourceSet' {} a -> s {tags = a} :: CreateResourceSet) Prelude.. Lens.mapping Lens.coerced

-- | AWS Resource type of the resources in the ResourceSet
createResourceSet_resourceSetType :: Lens.Lens' CreateResourceSet Prelude.Text
createResourceSet_resourceSetType = Lens.lens (\CreateResourceSet' {resourceSetType} -> resourceSetType) (\s@CreateResourceSet' {} a -> s {resourceSetType = a} :: CreateResourceSet)

-- | The name of the ResourceSet to create
createResourceSet_resourceSetName :: Lens.Lens' CreateResourceSet Prelude.Text
createResourceSet_resourceSetName = Lens.lens (\CreateResourceSet' {resourceSetName} -> resourceSetName) (\s@CreateResourceSet' {} a -> s {resourceSetName = a} :: CreateResourceSet)

-- | A list of Resource objects
createResourceSet_resources :: Lens.Lens' CreateResourceSet [Resource]
createResourceSet_resources = Lens.lens (\CreateResourceSet' {resources} -> resources) (\s@CreateResourceSet' {} a -> s {resources = a} :: CreateResourceSet) Prelude.. Lens.coerced

instance Core.AWSRequest CreateResourceSet where
  type
    AWSResponse CreateResourceSet =
      CreateResourceSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceSetResponse'
            Prelude.<$> (x Core..?> "resourceSetName")
            Prelude.<*> (x Core..?> "resourceSetType")
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceSetArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResourceSet where
  hashWithSalt salt' CreateResourceSet' {..} =
    salt' `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` resourceSetName
      `Prelude.hashWithSalt` resourceSetType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateResourceSet where
  rnf CreateResourceSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resourceSetType

instance Core.ToHeaders CreateResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateResourceSet where
  toJSON CreateResourceSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("resourceSetType" Core..= resourceSetType),
            Prelude.Just
              ("resourceSetName" Core..= resourceSetName),
            Prelude.Just ("resources" Core..= resources)
          ]
      )

instance Core.ToPath CreateResourceSet where
  toPath = Prelude.const "/resourcesets"

instance Core.ToQuery CreateResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceSetResponse' smart constructor.
data CreateResourceSetResponse = CreateResourceSetResponse'
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
-- Create a value of 'CreateResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'createResourceSetResponse_resourceSetName' - The name of the ResourceSet
--
-- 'resourceSetType', 'createResourceSetResponse_resourceSetType' - AWS Resource Type of the resources in the ResourceSet
--
-- 'resources', 'createResourceSetResponse_resources' - A list of Resource objects
--
-- 'resourceSetArn', 'createResourceSetResponse_resourceSetArn' - The arn for the ResourceSet
--
-- 'tags', 'createResourceSetResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createResourceSetResponse_httpStatus' - The response's http status code.
newCreateResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceSetResponse
newCreateResourceSetResponse pHttpStatus_ =
  CreateResourceSetResponse'
    { resourceSetName =
        Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resources = Prelude.Nothing,
      resourceSetArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the ResourceSet
createResourceSetResponse_resourceSetName :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetName = Lens.lens (\CreateResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@CreateResourceSetResponse' {} a -> s {resourceSetName = a} :: CreateResourceSetResponse)

-- | AWS Resource Type of the resources in the ResourceSet
createResourceSetResponse_resourceSetType :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetType = Lens.lens (\CreateResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@CreateResourceSetResponse' {} a -> s {resourceSetType = a} :: CreateResourceSetResponse)

-- | A list of Resource objects
createResourceSetResponse_resources :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe [Resource])
createResourceSetResponse_resources = Lens.lens (\CreateResourceSetResponse' {resources} -> resources) (\s@CreateResourceSetResponse' {} a -> s {resources = a} :: CreateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The arn for the ResourceSet
createResourceSetResponse_resourceSetArn :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetArn = Lens.lens (\CreateResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@CreateResourceSetResponse' {} a -> s {resourceSetArn = a} :: CreateResourceSetResponse)

-- | Undocumented member.
createResourceSetResponse_tags :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResourceSetResponse_tags = Lens.lens (\CreateResourceSetResponse' {tags} -> tags) (\s@CreateResourceSetResponse' {} a -> s {tags = a} :: CreateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createResourceSetResponse_httpStatus :: Lens.Lens' CreateResourceSetResponse Prelude.Int
createResourceSetResponse_httpStatus = Lens.lens (\CreateResourceSetResponse' {httpStatus} -> httpStatus) (\s@CreateResourceSetResponse' {} a -> s {httpStatus = a} :: CreateResourceSetResponse)

instance Prelude.NFData CreateResourceSetResponse where
  rnf CreateResourceSetResponse' {..} =
    Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceSetArn
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf resourceSetType
