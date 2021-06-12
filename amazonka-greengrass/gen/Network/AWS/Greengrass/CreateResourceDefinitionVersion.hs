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
-- Module      : Network.AWS.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been
-- defined.
module Network.AWS.Greengrass.CreateResourceDefinitionVersion
  ( -- * Creating a Request
    CreateResourceDefinitionVersion (..),
    newCreateResourceDefinitionVersion,

    -- * Request Lenses
    createResourceDefinitionVersion_resources,
    createResourceDefinitionVersion_amznClientToken,
    createResourceDefinitionVersion_resourceDefinitionId,

    -- * Destructuring the Response
    CreateResourceDefinitionVersionResponse (..),
    newCreateResourceDefinitionVersionResponse,

    -- * Response Lenses
    createResourceDefinitionVersionResponse_creationTimestamp,
    createResourceDefinitionVersionResponse_arn,
    createResourceDefinitionVersionResponse_id,
    createResourceDefinitionVersionResponse_version,
    createResourceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { -- | A list of resources.
    resources :: Core.Maybe [Resource],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'createResourceDefinitionVersion_resources' - A list of resources.
--
-- 'amznClientToken', 'createResourceDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'resourceDefinitionId', 'createResourceDefinitionVersion_resourceDefinitionId' - The ID of the resource definition.
newCreateResourceDefinitionVersion ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  CreateResourceDefinitionVersion
newCreateResourceDefinitionVersion
  pResourceDefinitionId_ =
    CreateResourceDefinitionVersion'
      { resources =
          Core.Nothing,
        amznClientToken = Core.Nothing,
        resourceDefinitionId =
          pResourceDefinitionId_
      }

-- | A list of resources.
createResourceDefinitionVersion_resources :: Lens.Lens' CreateResourceDefinitionVersion (Core.Maybe [Resource])
createResourceDefinitionVersion_resources = Lens.lens (\CreateResourceDefinitionVersion' {resources} -> resources) (\s@CreateResourceDefinitionVersion' {} a -> s {resources = a} :: CreateResourceDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createResourceDefinitionVersion_amznClientToken :: Lens.Lens' CreateResourceDefinitionVersion (Core.Maybe Core.Text)
createResourceDefinitionVersion_amznClientToken = Lens.lens (\CreateResourceDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateResourceDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateResourceDefinitionVersion)

-- | The ID of the resource definition.
createResourceDefinitionVersion_resourceDefinitionId :: Lens.Lens' CreateResourceDefinitionVersion Core.Text
createResourceDefinitionVersion_resourceDefinitionId = Lens.lens (\CreateResourceDefinitionVersion' {resourceDefinitionId} -> resourceDefinitionId) (\s@CreateResourceDefinitionVersion' {} a -> s {resourceDefinitionId = a} :: CreateResourceDefinitionVersion)

instance
  Core.AWSRequest
    CreateResourceDefinitionVersion
  where
  type
    AWSResponse CreateResourceDefinitionVersion =
      CreateResourceDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateResourceDefinitionVersion

instance Core.NFData CreateResourceDefinitionVersion

instance
  Core.ToHeaders
    CreateResourceDefinitionVersion
  where
  toHeaders CreateResourceDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateResourceDefinitionVersion where
  toJSON CreateResourceDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Resources" Core..=) Core.<$> resources]
      )

instance Core.ToPath CreateResourceDefinitionVersion where
  toPath CreateResourceDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId,
        "/versions"
      ]

instance Core.ToQuery CreateResourceDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createResourceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createResourceDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createResourceDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createResourceDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createResourceDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateResourceDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateResourceDefinitionVersionResponse
newCreateResourceDefinitionVersionResponse
  pHttpStatus_ =
    CreateResourceDefinitionVersionResponse'
      { creationTimestamp =
          Core.Nothing,
        arn = Core.Nothing,
        id = Core.Nothing,
        version = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
createResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateResourceDefinitionVersionResponse)

-- | The ARN of the version.
createResourceDefinitionVersionResponse_arn :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
createResourceDefinitionVersionResponse_arn = Lens.lens (\CreateResourceDefinitionVersionResponse' {arn} -> arn) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {arn = a} :: CreateResourceDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createResourceDefinitionVersionResponse_id :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
createResourceDefinitionVersionResponse_id = Lens.lens (\CreateResourceDefinitionVersionResponse' {id} -> id) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {id = a} :: CreateResourceDefinitionVersionResponse)

-- | The ID of the version.
createResourceDefinitionVersionResponse_version :: Lens.Lens' CreateResourceDefinitionVersionResponse (Core.Maybe Core.Text)
createResourceDefinitionVersionResponse_version = Lens.lens (\CreateResourceDefinitionVersionResponse' {version} -> version) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {version = a} :: CreateResourceDefinitionVersionResponse)

-- | The response's http status code.
createResourceDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateResourceDefinitionVersionResponse Core.Int
createResourceDefinitionVersionResponse_httpStatus = Lens.lens (\CreateResourceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateResourceDefinitionVersionResponse)

instance
  Core.NFData
    CreateResourceDefinitionVersionResponse
