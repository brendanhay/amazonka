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
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logger definition. You may provide the initial version of the
-- logger definition now or use \'\'CreateLoggerDefinitionVersion\'\' at a
-- later time.
module Network.AWS.Greengrass.CreateLoggerDefinition
  ( -- * Creating a Request
    CreateLoggerDefinition (..),
    newCreateLoggerDefinition,

    -- * Request Lenses
    createLoggerDefinition_name,
    createLoggerDefinition_initialVersion,
    createLoggerDefinition_tags,
    createLoggerDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateLoggerDefinitionResponse (..),
    newCreateLoggerDefinitionResponse,

    -- * Response Lenses
    createLoggerDefinitionResponse_creationTimestamp,
    createLoggerDefinitionResponse_latestVersionArn,
    createLoggerDefinitionResponse_latestVersion,
    createLoggerDefinitionResponse_arn,
    createLoggerDefinitionResponse_id,
    createLoggerDefinitionResponse_name,
    createLoggerDefinitionResponse_lastUpdatedTimestamp,
    createLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoggerDefinition' smart constructor.
data CreateLoggerDefinition = CreateLoggerDefinition'
  { -- | The name of the logger definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the logger definition.
    initialVersion :: Prelude.Maybe LoggerDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createLoggerDefinition_name' - The name of the logger definition.
--
-- 'initialVersion', 'createLoggerDefinition_initialVersion' - Information about the initial version of the logger definition.
--
-- 'tags', 'createLoggerDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createLoggerDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateLoggerDefinition ::
  CreateLoggerDefinition
newCreateLoggerDefinition =
  CreateLoggerDefinition'
    { name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the logger definition.
createLoggerDefinition_name :: Lens.Lens' CreateLoggerDefinition (Prelude.Maybe Prelude.Text)
createLoggerDefinition_name = Lens.lens (\CreateLoggerDefinition' {name} -> name) (\s@CreateLoggerDefinition' {} a -> s {name = a} :: CreateLoggerDefinition)

-- | Information about the initial version of the logger definition.
createLoggerDefinition_initialVersion :: Lens.Lens' CreateLoggerDefinition (Prelude.Maybe LoggerDefinitionVersion)
createLoggerDefinition_initialVersion = Lens.lens (\CreateLoggerDefinition' {initialVersion} -> initialVersion) (\s@CreateLoggerDefinition' {} a -> s {initialVersion = a} :: CreateLoggerDefinition)

-- | Tag(s) to add to the new resource.
createLoggerDefinition_tags :: Lens.Lens' CreateLoggerDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLoggerDefinition_tags = Lens.lens (\CreateLoggerDefinition' {tags} -> tags) (\s@CreateLoggerDefinition' {} a -> s {tags = a} :: CreateLoggerDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createLoggerDefinition_amznClientToken :: Lens.Lens' CreateLoggerDefinition (Prelude.Maybe Prelude.Text)
createLoggerDefinition_amznClientToken = Lens.lens (\CreateLoggerDefinition' {amznClientToken} -> amznClientToken) (\s@CreateLoggerDefinition' {} a -> s {amznClientToken = a} :: CreateLoggerDefinition)

instance Core.AWSRequest CreateLoggerDefinition where
  type
    AWSResponse CreateLoggerDefinition =
      CreateLoggerDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoggerDefinition

instance Prelude.NFData CreateLoggerDefinition

instance Core.ToHeaders CreateLoggerDefinition where
  toHeaders CreateLoggerDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateLoggerDefinition where
  toJSON CreateLoggerDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("InitialVersion" Core..=)
              Prelude.<$> initialVersion,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateLoggerDefinition where
  toPath =
    Prelude.const "/greengrass/definition/loggers"

instance Core.ToQuery CreateLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLoggerDefinitionResponse' smart constructor.
data CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createLoggerDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createLoggerDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createLoggerDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createLoggerDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createLoggerDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createLoggerDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createLoggerDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createLoggerDefinitionResponse_httpStatus' - The response's http status code.
newCreateLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoggerDefinitionResponse
newCreateLoggerDefinitionResponse pHttpStatus_ =
  CreateLoggerDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createLoggerDefinitionResponse_creationTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_creationTimestamp = Lens.lens (\CreateLoggerDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateLoggerDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateLoggerDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createLoggerDefinitionResponse_latestVersionArn :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_latestVersionArn = Lens.lens (\CreateLoggerDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateLoggerDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateLoggerDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createLoggerDefinitionResponse_latestVersion :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_latestVersion = Lens.lens (\CreateLoggerDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateLoggerDefinitionResponse' {} a -> s {latestVersion = a} :: CreateLoggerDefinitionResponse)

-- | The ARN of the definition.
createLoggerDefinitionResponse_arn :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_arn = Lens.lens (\CreateLoggerDefinitionResponse' {arn} -> arn) (\s@CreateLoggerDefinitionResponse' {} a -> s {arn = a} :: CreateLoggerDefinitionResponse)

-- | The ID of the definition.
createLoggerDefinitionResponse_id :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_id = Lens.lens (\CreateLoggerDefinitionResponse' {id} -> id) (\s@CreateLoggerDefinitionResponse' {} a -> s {id = a} :: CreateLoggerDefinitionResponse)

-- | The name of the definition.
createLoggerDefinitionResponse_name :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_name = Lens.lens (\CreateLoggerDefinitionResponse' {name} -> name) (\s@CreateLoggerDefinitionResponse' {} a -> s {name = a} :: CreateLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createLoggerDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
createLoggerDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateLoggerDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateLoggerDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateLoggerDefinitionResponse)

-- | The response's http status code.
createLoggerDefinitionResponse_httpStatus :: Lens.Lens' CreateLoggerDefinitionResponse Prelude.Int
createLoggerDefinitionResponse_httpStatus = Lens.lens (\CreateLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: CreateLoggerDefinitionResponse)

instance
  Prelude.NFData
    CreateLoggerDefinitionResponse
