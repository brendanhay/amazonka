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
-- Module      : Amazonka.Greengrass.CreateFunctionDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a Lambda function definition that has already been
-- defined.
module Amazonka.Greengrass.CreateFunctionDefinitionVersion
  ( -- * Creating a Request
    CreateFunctionDefinitionVersion (..),
    newCreateFunctionDefinitionVersion,

    -- * Request Lenses
    createFunctionDefinitionVersion_amznClientToken,
    createFunctionDefinitionVersion_defaultConfig,
    createFunctionDefinitionVersion_functions,
    createFunctionDefinitionVersion_functionDefinitionId,

    -- * Destructuring the Response
    CreateFunctionDefinitionVersionResponse (..),
    newCreateFunctionDefinitionVersionResponse,

    -- * Response Lenses
    createFunctionDefinitionVersionResponse_arn,
    createFunctionDefinitionVersionResponse_creationTimestamp,
    createFunctionDefinitionVersionResponse_id,
    createFunctionDefinitionVersionResponse_version,
    createFunctionDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Information needed to create a function definition version.
--
-- /See:/ 'newCreateFunctionDefinitionVersion' smart constructor.
data CreateFunctionDefinitionVersion = CreateFunctionDefinitionVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The default configuration that applies to all Lambda functions in this
    -- function definition version. Individual Lambda functions can override
    -- these settings.
    defaultConfig :: Prelude.Maybe FunctionDefaultConfig,
    -- | A list of Lambda functions in this function definition version.
    functions :: Prelude.Maybe [Function],
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createFunctionDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'defaultConfig', 'createFunctionDefinitionVersion_defaultConfig' - The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
--
-- 'functions', 'createFunctionDefinitionVersion_functions' - A list of Lambda functions in this function definition version.
--
-- 'functionDefinitionId', 'createFunctionDefinitionVersion_functionDefinitionId' - The ID of the Lambda function definition.
newCreateFunctionDefinitionVersion ::
  -- | 'functionDefinitionId'
  Prelude.Text ->
  CreateFunctionDefinitionVersion
newCreateFunctionDefinitionVersion
  pFunctionDefinitionId_ =
    CreateFunctionDefinitionVersion'
      { amznClientToken =
          Prelude.Nothing,
        defaultConfig = Prelude.Nothing,
        functions = Prelude.Nothing,
        functionDefinitionId =
          pFunctionDefinitionId_
      }

-- | A client token used to correlate requests and responses.
createFunctionDefinitionVersion_amznClientToken :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersion_amznClientToken = Lens.lens (\CreateFunctionDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateFunctionDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateFunctionDefinitionVersion)

-- | The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
createFunctionDefinitionVersion_defaultConfig :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe FunctionDefaultConfig)
createFunctionDefinitionVersion_defaultConfig = Lens.lens (\CreateFunctionDefinitionVersion' {defaultConfig} -> defaultConfig) (\s@CreateFunctionDefinitionVersion' {} a -> s {defaultConfig = a} :: CreateFunctionDefinitionVersion)

-- | A list of Lambda functions in this function definition version.
createFunctionDefinitionVersion_functions :: Lens.Lens' CreateFunctionDefinitionVersion (Prelude.Maybe [Function])
createFunctionDefinitionVersion_functions = Lens.lens (\CreateFunctionDefinitionVersion' {functions} -> functions) (\s@CreateFunctionDefinitionVersion' {} a -> s {functions = a} :: CreateFunctionDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Lambda function definition.
createFunctionDefinitionVersion_functionDefinitionId :: Lens.Lens' CreateFunctionDefinitionVersion Prelude.Text
createFunctionDefinitionVersion_functionDefinitionId = Lens.lens (\CreateFunctionDefinitionVersion' {functionDefinitionId} -> functionDefinitionId) (\s@CreateFunctionDefinitionVersion' {} a -> s {functionDefinitionId = a} :: CreateFunctionDefinitionVersion)

instance
  Core.AWSRequest
    CreateFunctionDefinitionVersion
  where
  type
    AWSResponse CreateFunctionDefinitionVersion =
      CreateFunctionDefinitionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateFunctionDefinitionVersion
  where
  hashWithSalt
    _salt
    CreateFunctionDefinitionVersion' {..} =
      _salt
        `Prelude.hashWithSalt` amznClientToken
        `Prelude.hashWithSalt` defaultConfig
        `Prelude.hashWithSalt` functions
        `Prelude.hashWithSalt` functionDefinitionId

instance
  Prelude.NFData
    CreateFunctionDefinitionVersion
  where
  rnf CreateFunctionDefinitionVersion' {..} =
    Prelude.rnf amznClientToken `Prelude.seq`
      Prelude.rnf defaultConfig `Prelude.seq`
        Prelude.rnf functions `Prelude.seq`
          Prelude.rnf functionDefinitionId

instance
  Data.ToHeaders
    CreateFunctionDefinitionVersion
  where
  toHeaders CreateFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateFunctionDefinitionVersion where
  toJSON CreateFunctionDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultConfig" Data..=) Prelude.<$> defaultConfig,
            ("Functions" Data..=) Prelude.<$> functions
          ]
      )

instance Data.ToPath CreateFunctionDefinitionVersion where
  toPath CreateFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Data.toBS functionDefinitionId,
        "/versions"
      ]

instance Data.ToQuery CreateFunctionDefinitionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFunctionDefinitionVersionResponse' smart constructor.
data CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFunctionDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'creationTimestamp', 'createFunctionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
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
      { arn =
          Prelude.Nothing,
        creationTimestamp =
          Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the version.
createFunctionDefinitionVersionResponse_arn :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_arn = Lens.lens (\CreateFunctionDefinitionVersionResponse' {arn} -> arn) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {arn = a} :: CreateFunctionDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the version was created.
createFunctionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateFunctionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateFunctionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateFunctionDefinitionVersionResponse)

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
  where
  rnf CreateFunctionDefinitionVersionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf version `Prelude.seq`
            Prelude.rnf httpStatus
