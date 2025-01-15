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
-- Module      : Amazonka.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda
-- functions and their configurations to be used in a group. You can create
-- an initial version of the definition by providing a list of Lambda
-- functions and their configurations now, or use
-- \'\'CreateFunctionDefinitionVersion\'\' later.
module Amazonka.Greengrass.CreateFunctionDefinition
  ( -- * Creating a Request
    CreateFunctionDefinition (..),
    newCreateFunctionDefinition,

    -- * Request Lenses
    createFunctionDefinition_amznClientToken,
    createFunctionDefinition_initialVersion,
    createFunctionDefinition_name,
    createFunctionDefinition_tags,

    -- * Destructuring the Response
    CreateFunctionDefinitionResponse (..),
    newCreateFunctionDefinitionResponse,

    -- * Response Lenses
    createFunctionDefinitionResponse_arn,
    createFunctionDefinitionResponse_creationTimestamp,
    createFunctionDefinitionResponse_id,
    createFunctionDefinitionResponse_lastUpdatedTimestamp,
    createFunctionDefinitionResponse_latestVersion,
    createFunctionDefinitionResponse_latestVersionArn,
    createFunctionDefinitionResponse_name,
    createFunctionDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the function definition.
    initialVersion :: Prelude.Maybe FunctionDefinitionVersion,
    -- | The name of the function definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createFunctionDefinition_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'initialVersion', 'createFunctionDefinition_initialVersion' - Information about the initial version of the function definition.
--
-- 'name', 'createFunctionDefinition_name' - The name of the function definition.
--
-- 'tags', 'createFunctionDefinition_tags' - Tag(s) to add to the new resource.
newCreateFunctionDefinition ::
  CreateFunctionDefinition
newCreateFunctionDefinition =
  CreateFunctionDefinition'
    { amznClientToken =
        Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A client token used to correlate requests and responses.
createFunctionDefinition_amznClientToken :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe Prelude.Text)
createFunctionDefinition_amznClientToken = Lens.lens (\CreateFunctionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateFunctionDefinition' {} a -> s {amznClientToken = a} :: CreateFunctionDefinition)

-- | Information about the initial version of the function definition.
createFunctionDefinition_initialVersion :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe FunctionDefinitionVersion)
createFunctionDefinition_initialVersion = Lens.lens (\CreateFunctionDefinition' {initialVersion} -> initialVersion) (\s@CreateFunctionDefinition' {} a -> s {initialVersion = a} :: CreateFunctionDefinition)

-- | The name of the function definition.
createFunctionDefinition_name :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe Prelude.Text)
createFunctionDefinition_name = Lens.lens (\CreateFunctionDefinition' {name} -> name) (\s@CreateFunctionDefinition' {} a -> s {name = a} :: CreateFunctionDefinition)

-- | Tag(s) to add to the new resource.
createFunctionDefinition_tags :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFunctionDefinition_tags = Lens.lens (\CreateFunctionDefinition' {tags} -> tags) (\s@CreateFunctionDefinition' {} a -> s {tags = a} :: CreateFunctionDefinition) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateFunctionDefinition where
  type
    AWSResponse CreateFunctionDefinition =
      CreateFunctionDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFunctionDefinition where
  hashWithSalt _salt CreateFunctionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` amznClientToken
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateFunctionDefinition where
  rnf CreateFunctionDefinition' {..} =
    Prelude.rnf amznClientToken `Prelude.seq`
      Prelude.rnf initialVersion `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf tags

instance Data.ToHeaders CreateFunctionDefinition where
  toHeaders CreateFunctionDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateFunctionDefinition where
  toJSON CreateFunctionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InitialVersion" Data..=)
              Prelude.<$> initialVersion,
            ("Name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateFunctionDefinition where
  toPath =
    Prelude.const "/greengrass/definition/functions"

instance Data.ToQuery CreateFunctionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
  { -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFunctionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'createFunctionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'createFunctionDefinitionResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'createFunctionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'createFunctionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'createFunctionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'createFunctionDefinitionResponse_name' - The name of the definition.
--
-- 'httpStatus', 'createFunctionDefinitionResponse_httpStatus' - The response's http status code.
newCreateFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFunctionDefinitionResponse
newCreateFunctionDefinitionResponse pHttpStatus_ =
  CreateFunctionDefinitionResponse'
    { arn =
        Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the definition.
createFunctionDefinitionResponse_arn :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_arn = Lens.lens (\CreateFunctionDefinitionResponse' {arn} -> arn) (\s@CreateFunctionDefinitionResponse' {} a -> s {arn = a} :: CreateFunctionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createFunctionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_creationTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the definition.
createFunctionDefinitionResponse_id :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_id = Lens.lens (\CreateFunctionDefinitionResponse' {id} -> id) (\s@CreateFunctionDefinitionResponse' {} a -> s {id = a} :: CreateFunctionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createFunctionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_latestVersion = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateFunctionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_latestVersionArn = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateFunctionDefinitionResponse)

-- | The name of the definition.
createFunctionDefinitionResponse_name :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_name = Lens.lens (\CreateFunctionDefinitionResponse' {name} -> name) (\s@CreateFunctionDefinitionResponse' {} a -> s {name = a} :: CreateFunctionDefinitionResponse)

-- | The response's http status code.
createFunctionDefinitionResponse_httpStatus :: Lens.Lens' CreateFunctionDefinitionResponse Prelude.Int
createFunctionDefinitionResponse_httpStatus = Lens.lens (\CreateFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateFunctionDefinitionResponse)

instance
  Prelude.NFData
    CreateFunctionDefinitionResponse
  where
  rnf CreateFunctionDefinitionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastUpdatedTimestamp `Prelude.seq`
            Prelude.rnf latestVersion `Prelude.seq`
              Prelude.rnf latestVersionArn `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf httpStatus
