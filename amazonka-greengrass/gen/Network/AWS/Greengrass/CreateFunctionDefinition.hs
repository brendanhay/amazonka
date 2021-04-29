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
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda
-- functions and their configurations to be used in a group. You can create
-- an initial version of the definition by providing a list of Lambda
-- functions and their configurations now, or use
-- \'\'CreateFunctionDefinitionVersion\'\' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
  ( -- * Creating a Request
    CreateFunctionDefinition (..),
    newCreateFunctionDefinition,

    -- * Request Lenses
    createFunctionDefinition_name,
    createFunctionDefinition_initialVersion,
    createFunctionDefinition_tags,
    createFunctionDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateFunctionDefinitionResponse (..),
    newCreateFunctionDefinitionResponse,

    -- * Response Lenses
    createFunctionDefinitionResponse_creationTimestamp,
    createFunctionDefinitionResponse_latestVersionArn,
    createFunctionDefinitionResponse_latestVersion,
    createFunctionDefinitionResponse_arn,
    createFunctionDefinitionResponse_id,
    createFunctionDefinitionResponse_name,
    createFunctionDefinitionResponse_lastUpdatedTimestamp,
    createFunctionDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { -- | The name of the function definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the function definition.
    initialVersion :: Prelude.Maybe FunctionDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createFunctionDefinition_name' - The name of the function definition.
--
-- 'initialVersion', 'createFunctionDefinition_initialVersion' - Information about the initial version of the function definition.
--
-- 'tags', 'createFunctionDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createFunctionDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateFunctionDefinition ::
  CreateFunctionDefinition
newCreateFunctionDefinition =
  CreateFunctionDefinition'
    { name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the function definition.
createFunctionDefinition_name :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe Prelude.Text)
createFunctionDefinition_name = Lens.lens (\CreateFunctionDefinition' {name} -> name) (\s@CreateFunctionDefinition' {} a -> s {name = a} :: CreateFunctionDefinition)

-- | Information about the initial version of the function definition.
createFunctionDefinition_initialVersion :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe FunctionDefinitionVersion)
createFunctionDefinition_initialVersion = Lens.lens (\CreateFunctionDefinition' {initialVersion} -> initialVersion) (\s@CreateFunctionDefinition' {} a -> s {initialVersion = a} :: CreateFunctionDefinition)

-- | Tag(s) to add to the new resource.
createFunctionDefinition_tags :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFunctionDefinition_tags = Lens.lens (\CreateFunctionDefinition' {tags} -> tags) (\s@CreateFunctionDefinition' {} a -> s {tags = a} :: CreateFunctionDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createFunctionDefinition_amznClientToken :: Lens.Lens' CreateFunctionDefinition (Prelude.Maybe Prelude.Text)
createFunctionDefinition_amznClientToken = Lens.lens (\CreateFunctionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateFunctionDefinition' {} a -> s {amznClientToken = a} :: CreateFunctionDefinition)

instance Prelude.AWSRequest CreateFunctionDefinition where
  type
    Rs CreateFunctionDefinition =
      CreateFunctionDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "LatestVersionArn")
            Prelude.<*> (x Prelude..?> "LatestVersion")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFunctionDefinition

instance Prelude.NFData CreateFunctionDefinition

instance Prelude.ToHeaders CreateFunctionDefinition where
  toHeaders CreateFunctionDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateFunctionDefinition where
  toJSON CreateFunctionDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("InitialVersion" Prelude..=)
              Prelude.<$> initialVersion,
            ("tags" Prelude..=) Prelude.<$> tags
          ]
      )

instance Prelude.ToPath CreateFunctionDefinition where
  toPath =
    Prelude.const "/greengrass/definition/functions"

instance Prelude.ToQuery CreateFunctionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createFunctionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createFunctionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createFunctionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createFunctionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createFunctionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createFunctionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createFunctionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createFunctionDefinitionResponse_httpStatus' - The response's http status code.
newCreateFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFunctionDefinitionResponse
newCreateFunctionDefinitionResponse pHttpStatus_ =
  CreateFunctionDefinitionResponse'
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
createFunctionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_creationTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_latestVersionArn = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createFunctionDefinitionResponse_latestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_latestVersion = Lens.lens (\CreateFunctionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateFunctionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateFunctionDefinitionResponse)

-- | The ARN of the definition.
createFunctionDefinitionResponse_arn :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_arn = Lens.lens (\CreateFunctionDefinitionResponse' {arn} -> arn) (\s@CreateFunctionDefinitionResponse' {} a -> s {arn = a} :: CreateFunctionDefinitionResponse)

-- | The ID of the definition.
createFunctionDefinitionResponse_id :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_id = Lens.lens (\CreateFunctionDefinitionResponse' {id} -> id) (\s@CreateFunctionDefinitionResponse' {} a -> s {id = a} :: CreateFunctionDefinitionResponse)

-- | The name of the definition.
createFunctionDefinitionResponse_name :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_name = Lens.lens (\CreateFunctionDefinitionResponse' {name} -> name) (\s@CreateFunctionDefinitionResponse' {} a -> s {name = a} :: CreateFunctionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createFunctionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
createFunctionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateFunctionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateFunctionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateFunctionDefinitionResponse)

-- | The response's http status code.
createFunctionDefinitionResponse_httpStatus :: Lens.Lens' CreateFunctionDefinitionResponse Prelude.Int
createFunctionDefinitionResponse_httpStatus = Lens.lens (\CreateFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateFunctionDefinitionResponse)

instance
  Prelude.NFData
    CreateFunctionDefinitionResponse
