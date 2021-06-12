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
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a logger definition that has already been defined.
module Network.AWS.Greengrass.CreateLoggerDefinitionVersion
  ( -- * Creating a Request
    CreateLoggerDefinitionVersion (..),
    newCreateLoggerDefinitionVersion,

    -- * Request Lenses
    createLoggerDefinitionVersion_loggers,
    createLoggerDefinitionVersion_amznClientToken,
    createLoggerDefinitionVersion_loggerDefinitionId,

    -- * Destructuring the Response
    CreateLoggerDefinitionVersionResponse (..),
    newCreateLoggerDefinitionVersionResponse,

    -- * Response Lenses
    createLoggerDefinitionVersionResponse_creationTimestamp,
    createLoggerDefinitionVersionResponse_arn,
    createLoggerDefinitionVersionResponse_id,
    createLoggerDefinitionVersionResponse_version,
    createLoggerDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoggerDefinitionVersion' smart constructor.
data CreateLoggerDefinitionVersion = CreateLoggerDefinitionVersion'
  { -- | A list of loggers.
    loggers :: Core.Maybe [GreengrassLogger],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoggerDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggers', 'createLoggerDefinitionVersion_loggers' - A list of loggers.
--
-- 'amznClientToken', 'createLoggerDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'loggerDefinitionId', 'createLoggerDefinitionVersion_loggerDefinitionId' - The ID of the logger definition.
newCreateLoggerDefinitionVersion ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  CreateLoggerDefinitionVersion
newCreateLoggerDefinitionVersion pLoggerDefinitionId_ =
  CreateLoggerDefinitionVersion'
    { loggers =
        Core.Nothing,
      amznClientToken = Core.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | A list of loggers.
createLoggerDefinitionVersion_loggers :: Lens.Lens' CreateLoggerDefinitionVersion (Core.Maybe [GreengrassLogger])
createLoggerDefinitionVersion_loggers = Lens.lens (\CreateLoggerDefinitionVersion' {loggers} -> loggers) (\s@CreateLoggerDefinitionVersion' {} a -> s {loggers = a} :: CreateLoggerDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createLoggerDefinitionVersion_amznClientToken :: Lens.Lens' CreateLoggerDefinitionVersion (Core.Maybe Core.Text)
createLoggerDefinitionVersion_amznClientToken = Lens.lens (\CreateLoggerDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateLoggerDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateLoggerDefinitionVersion)

-- | The ID of the logger definition.
createLoggerDefinitionVersion_loggerDefinitionId :: Lens.Lens' CreateLoggerDefinitionVersion Core.Text
createLoggerDefinitionVersion_loggerDefinitionId = Lens.lens (\CreateLoggerDefinitionVersion' {loggerDefinitionId} -> loggerDefinitionId) (\s@CreateLoggerDefinitionVersion' {} a -> s {loggerDefinitionId = a} :: CreateLoggerDefinitionVersion)

instance
  Core.AWSRequest
    CreateLoggerDefinitionVersion
  where
  type
    AWSResponse CreateLoggerDefinitionVersion =
      CreateLoggerDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLoggerDefinitionVersion

instance Core.NFData CreateLoggerDefinitionVersion

instance Core.ToHeaders CreateLoggerDefinitionVersion where
  toHeaders CreateLoggerDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateLoggerDefinitionVersion where
  toJSON CreateLoggerDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Loggers" Core..=) Core.<$> loggers]
      )

instance Core.ToPath CreateLoggerDefinitionVersion where
  toPath CreateLoggerDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId,
        "/versions"
      ]

instance Core.ToQuery CreateLoggerDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateLoggerDefinitionVersionResponse' smart constructor.
data CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse'
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
-- Create a value of 'CreateLoggerDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createLoggerDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createLoggerDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createLoggerDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createLoggerDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createLoggerDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateLoggerDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLoggerDefinitionVersionResponse
newCreateLoggerDefinitionVersionResponse pHttpStatus_ =
  CreateLoggerDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createLoggerDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
createLoggerDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateLoggerDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateLoggerDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateLoggerDefinitionVersionResponse)

-- | The ARN of the version.
createLoggerDefinitionVersionResponse_arn :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
createLoggerDefinitionVersionResponse_arn = Lens.lens (\CreateLoggerDefinitionVersionResponse' {arn} -> arn) (\s@CreateLoggerDefinitionVersionResponse' {} a -> s {arn = a} :: CreateLoggerDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createLoggerDefinitionVersionResponse_id :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
createLoggerDefinitionVersionResponse_id = Lens.lens (\CreateLoggerDefinitionVersionResponse' {id} -> id) (\s@CreateLoggerDefinitionVersionResponse' {} a -> s {id = a} :: CreateLoggerDefinitionVersionResponse)

-- | The ID of the version.
createLoggerDefinitionVersionResponse_version :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
createLoggerDefinitionVersionResponse_version = Lens.lens (\CreateLoggerDefinitionVersionResponse' {version} -> version) (\s@CreateLoggerDefinitionVersionResponse' {} a -> s {version = a} :: CreateLoggerDefinitionVersionResponse)

-- | The response's http status code.
createLoggerDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateLoggerDefinitionVersionResponse Core.Int
createLoggerDefinitionVersionResponse_httpStatus = Lens.lens (\CreateLoggerDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateLoggerDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateLoggerDefinitionVersionResponse)

instance
  Core.NFData
    CreateLoggerDefinitionVersionResponse
