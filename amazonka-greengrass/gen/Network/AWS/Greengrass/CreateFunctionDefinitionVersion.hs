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
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a Lambda function definition that has already been
-- defined.
module Network.AWS.Greengrass.CreateFunctionDefinitionVersion
  ( -- * Creating a Request
    CreateFunctionDefinitionVersion (..),
    newCreateFunctionDefinitionVersion,

    -- * Request Lenses
    createFunctionDefinitionVersion_functions,
    createFunctionDefinitionVersion_defaultConfig,
    createFunctionDefinitionVersion_amznClientToken,
    createFunctionDefinitionVersion_functionDefinitionId,

    -- * Destructuring the Response
    CreateFunctionDefinitionVersionResponse (..),
    newCreateFunctionDefinitionVersionResponse,

    -- * Response Lenses
    createFunctionDefinitionVersionResponse_creationTimestamp,
    createFunctionDefinitionVersionResponse_arn,
    createFunctionDefinitionVersionResponse_id,
    createFunctionDefinitionVersionResponse_version,
    createFunctionDefinitionVersionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to create a function definition version.
--
-- /See:/ 'newCreateFunctionDefinitionVersion' smart constructor.
data CreateFunctionDefinitionVersion = CreateFunctionDefinitionVersion'
  { -- | A list of Lambda functions in this function definition version.
    functions :: Prelude.Maybe [Function],
    -- | The default configuration that applies to all Lambda functions in this
    -- function definition version. Individual Lambda functions can override
    -- these settings.
    defaultConfig :: Prelude.Maybe FunctionDefaultConfig,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functions', 'createFunctionDefinitionVersion_functions' - A list of Lambda functions in this function definition version.
--
-- 'defaultConfig', 'createFunctionDefinitionVersion_defaultConfig' - The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
--
-- 'amznClientToken', 'createFunctionDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'functionDefinitionId', 'createFunctionDefinitionVersion_functionDefinitionId' - The ID of the Lambda function definition.
newCreateFunctionDefinitionVersion ::
  -- | 'functionDefinitionId'
  Prelude.Text ->
  CreateFunctionDefinitionVersion
newCreateFunctionDefinitionVersion
  pFunctionDefinitionId_ =
    CreateFunctionDefinitionVersion'
      { functions =
          Prelude.Nothing,
        defaultConfig = Prelude.Nothing,
        amznClientToken = Prelude.Nothing,
        functionDefinitionId =
          pFunctionDefinitionId_
      }

-- | A list of Lambda functions in this function definition version.
createFunctionDefinitionVersion_functions :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe [Function])
createFunctionDefinitionVersion_functions = Lens.lens (\CreateFunctionDefinitionVersion' {functions} -> functions) (\s@CreateFunctionDefinitionVersion' {} a -> s {functions = a} :: CreateFunctionDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

-- | The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
createFunctionDefinitionVersion_defaultConfig :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe FunctionDefaultConfig)
createFunctionDefinitionVersion_defaultConfig = Lens.lens (\CreateFunctionDefinitionVersion' {defaultConfig} -> defaultConfig) (\s@CreateFunctionDefinitionVersion' {} a -> s {defaultConfig = a} :: CreateFunctionDefinitionVersion)

-- | A client token used to correlate requests and responses.
createFunctionDefinitionVersion_amznClientToken :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersion_amznClientToken = Lens.lens (\CreateFunctionDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateFunctionDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateFunctionDefinitionVersion)

-- | The ID of the Lambda function definition.
createFunctionDefinitionVersion_functionDefinitionId :: Lens.Lens' CreateFunctionDefinitionVersion Prelude.Text
createFunctionDefinitionVersion_functionDefinitionId = Lens.lens (\CreateFunctionDefinitionVersion' {functionDefinitionId} -> functionDefinitionId) (\s@CreateFunctionDefinitionVersion' {} a -> s {functionDefinitionId = a} :: CreateFunctionDefinitionVersion)

instance
  Prelude.AWSRequest
    CreateFunctionDefinitionVersion
  where
  type
    Rs CreateFunctionDefinitionVersion =
      CreateFunctionDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateFunctionDefinitionVersion

instance
  Prelude.NFData
    CreateFunctionDefinitionVersion

instance
  Prelude.ToHeaders
    CreateFunctionDefinitionVersion
  where
  toHeaders CreateFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Prelude.ToJSON
    CreateFunctionDefinitionVersion
  where
  toJSON CreateFunctionDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Functions" Prelude..=) Prelude.<$> functions,
            ("DefaultConfig" Prelude..=)
              Prelude.<$> defaultConfig
          ]
      )

instance
  Prelude.ToPath
    CreateFunctionDefinitionVersion
  where
  toPath CreateFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Prelude.toBS functionDefinitionId,
        "/versions"
      ]

instance
  Prelude.ToQuery
    CreateFunctionDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFunctionDefinitionVersionResponse' smart constructor.
data CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createFunctionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createFunctionDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createFunctionDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createFunctionDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createFunctionDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateFunctionDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFunctionDefinitionVersionResponse
newCreateFunctionDefinitionVersionResponse
  pHttpStatus_ =
    CreateFunctionDefinitionVersionResponse'
      { creationTimestamp =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createFunctionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateFunctionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateFunctionDefinitionVersionResponse)

-- | The ARN of the version.
createFunctionDefinitionVersionResponse_arn :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_arn = Lens.lens (\CreateFunctionDefinitionVersionResponse' {arn} -> arn) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {arn = a} :: CreateFunctionDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createFunctionDefinitionVersionResponse_id :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_id = Lens.lens (\CreateFunctionDefinitionVersionResponse' {id} -> id) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {id = a} :: CreateFunctionDefinitionVersionResponse)

-- | The ID of the version.
createFunctionDefinitionVersionResponse_version :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_version = Lens.lens (\CreateFunctionDefinitionVersionResponse' {version} -> version) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {version = a} :: CreateFunctionDefinitionVersionResponse)

-- | The response's http status code.
createFunctionDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateFunctionDefinitionVersionResponse Prelude.Int
createFunctionDefinitionVersionResponse_httpStatus = Lens.lens (\CreateFunctionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateFunctionDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateFunctionDefinitionVersionResponse
