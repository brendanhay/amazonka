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
-- Module      : Network.AWS.Greengrass.CreateCoreDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a core definition that has already been defined.
-- Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinitionVersion
  ( -- * Creating a Request
    CreateCoreDefinitionVersion (..),
    newCreateCoreDefinitionVersion,

    -- * Request Lenses
    createCoreDefinitionVersion_cores,
    createCoreDefinitionVersion_amznClientToken,
    createCoreDefinitionVersion_coreDefinitionId,

    -- * Destructuring the Response
    CreateCoreDefinitionVersionResponse (..),
    newCreateCoreDefinitionVersionResponse,

    -- * Response Lenses
    createCoreDefinitionVersionResponse_creationTimestamp,
    createCoreDefinitionVersionResponse_arn,
    createCoreDefinitionVersionResponse_id,
    createCoreDefinitionVersionResponse_version,
    createCoreDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCoreDefinitionVersion' smart constructor.
data CreateCoreDefinitionVersion = CreateCoreDefinitionVersion'
  { -- | A list of cores in the core definition version.
    cores :: Core.Maybe [Core],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the core definition.
    coreDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCoreDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cores', 'createCoreDefinitionVersion_cores' - A list of cores in the core definition version.
--
-- 'amznClientToken', 'createCoreDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'coreDefinitionId', 'createCoreDefinitionVersion_coreDefinitionId' - The ID of the core definition.
newCreateCoreDefinitionVersion ::
  -- | 'coreDefinitionId'
  Core.Text ->
  CreateCoreDefinitionVersion
newCreateCoreDefinitionVersion pCoreDefinitionId_ =
  CreateCoreDefinitionVersion'
    { cores = Core.Nothing,
      amznClientToken = Core.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | A list of cores in the core definition version.
createCoreDefinitionVersion_cores :: Lens.Lens' CreateCoreDefinitionVersion (Core.Maybe [Core])
createCoreDefinitionVersion_cores = Lens.lens (\CreateCoreDefinitionVersion' {cores} -> cores) (\s@CreateCoreDefinitionVersion' {} a -> s {cores = a} :: CreateCoreDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createCoreDefinitionVersion_amznClientToken :: Lens.Lens' CreateCoreDefinitionVersion (Core.Maybe Core.Text)
createCoreDefinitionVersion_amznClientToken = Lens.lens (\CreateCoreDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateCoreDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateCoreDefinitionVersion)

-- | The ID of the core definition.
createCoreDefinitionVersion_coreDefinitionId :: Lens.Lens' CreateCoreDefinitionVersion Core.Text
createCoreDefinitionVersion_coreDefinitionId = Lens.lens (\CreateCoreDefinitionVersion' {coreDefinitionId} -> coreDefinitionId) (\s@CreateCoreDefinitionVersion' {} a -> s {coreDefinitionId = a} :: CreateCoreDefinitionVersion)

instance Core.AWSRequest CreateCoreDefinitionVersion where
  type
    AWSResponse CreateCoreDefinitionVersion =
      CreateCoreDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCoreDefinitionVersion

instance Core.NFData CreateCoreDefinitionVersion

instance Core.ToHeaders CreateCoreDefinitionVersion where
  toHeaders CreateCoreDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateCoreDefinitionVersion where
  toJSON CreateCoreDefinitionVersion' {..} =
    Core.object
      (Core.catMaybes [("Cores" Core..=) Core.<$> cores])

instance Core.ToPath CreateCoreDefinitionVersion where
  toPath CreateCoreDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId,
        "/versions"
      ]

instance Core.ToQuery CreateCoreDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCoreDefinitionVersionResponse' smart constructor.
data CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse'
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
-- Create a value of 'CreateCoreDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createCoreDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createCoreDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createCoreDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createCoreDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createCoreDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateCoreDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCoreDefinitionVersionResponse
newCreateCoreDefinitionVersionResponse pHttpStatus_ =
  CreateCoreDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createCoreDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
createCoreDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateCoreDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateCoreDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateCoreDefinitionVersionResponse)

-- | The ARN of the version.
createCoreDefinitionVersionResponse_arn :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
createCoreDefinitionVersionResponse_arn = Lens.lens (\CreateCoreDefinitionVersionResponse' {arn} -> arn) (\s@CreateCoreDefinitionVersionResponse' {} a -> s {arn = a} :: CreateCoreDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createCoreDefinitionVersionResponse_id :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
createCoreDefinitionVersionResponse_id = Lens.lens (\CreateCoreDefinitionVersionResponse' {id} -> id) (\s@CreateCoreDefinitionVersionResponse' {} a -> s {id = a} :: CreateCoreDefinitionVersionResponse)

-- | The ID of the version.
createCoreDefinitionVersionResponse_version :: Lens.Lens' CreateCoreDefinitionVersionResponse (Core.Maybe Core.Text)
createCoreDefinitionVersionResponse_version = Lens.lens (\CreateCoreDefinitionVersionResponse' {version} -> version) (\s@CreateCoreDefinitionVersionResponse' {} a -> s {version = a} :: CreateCoreDefinitionVersionResponse)

-- | The response's http status code.
createCoreDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateCoreDefinitionVersionResponse Core.Int
createCoreDefinitionVersionResponse_httpStatus = Lens.lens (\CreateCoreDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateCoreDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateCoreDefinitionVersionResponse)

instance
  Core.NFData
    CreateCoreDefinitionVersionResponse
