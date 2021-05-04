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
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of
-- the connector definition now or use
-- \'\'CreateConnectorDefinitionVersion\'\' at a later time.
module Network.AWS.Greengrass.CreateConnectorDefinition
  ( -- * Creating a Request
    CreateConnectorDefinition (..),
    newCreateConnectorDefinition,

    -- * Request Lenses
    createConnectorDefinition_name,
    createConnectorDefinition_initialVersion,
    createConnectorDefinition_tags,
    createConnectorDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateConnectorDefinitionResponse (..),
    newCreateConnectorDefinitionResponse,

    -- * Response Lenses
    createConnectorDefinitionResponse_creationTimestamp,
    createConnectorDefinitionResponse_latestVersionArn,
    createConnectorDefinitionResponse_latestVersion,
    createConnectorDefinitionResponse_arn,
    createConnectorDefinitionResponse_id,
    createConnectorDefinitionResponse_name,
    createConnectorDefinitionResponse_lastUpdatedTimestamp,
    createConnectorDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { -- | The name of the connector definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the connector definition.
    initialVersion :: Prelude.Maybe ConnectorDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createConnectorDefinition_name' - The name of the connector definition.
--
-- 'initialVersion', 'createConnectorDefinition_initialVersion' - Information about the initial version of the connector definition.
--
-- 'tags', 'createConnectorDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createConnectorDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateConnectorDefinition ::
  CreateConnectorDefinition
newCreateConnectorDefinition =
  CreateConnectorDefinition'
    { name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the connector definition.
createConnectorDefinition_name :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe Prelude.Text)
createConnectorDefinition_name = Lens.lens (\CreateConnectorDefinition' {name} -> name) (\s@CreateConnectorDefinition' {} a -> s {name = a} :: CreateConnectorDefinition)

-- | Information about the initial version of the connector definition.
createConnectorDefinition_initialVersion :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe ConnectorDefinitionVersion)
createConnectorDefinition_initialVersion = Lens.lens (\CreateConnectorDefinition' {initialVersion} -> initialVersion) (\s@CreateConnectorDefinition' {} a -> s {initialVersion = a} :: CreateConnectorDefinition)

-- | Tag(s) to add to the new resource.
createConnectorDefinition_tags :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConnectorDefinition_tags = Lens.lens (\CreateConnectorDefinition' {tags} -> tags) (\s@CreateConnectorDefinition' {} a -> s {tags = a} :: CreateConnectorDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createConnectorDefinition_amznClientToken :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe Prelude.Text)
createConnectorDefinition_amznClientToken = Lens.lens (\CreateConnectorDefinition' {amznClientToken} -> amznClientToken) (\s@CreateConnectorDefinition' {} a -> s {amznClientToken = a} :: CreateConnectorDefinition)

instance Prelude.AWSRequest CreateConnectorDefinition where
  type
    Rs CreateConnectorDefinition =
      CreateConnectorDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "LatestVersionArn")
            Prelude.<*> (x Prelude..?> "LatestVersion")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectorDefinition

instance Prelude.NFData CreateConnectorDefinition

instance Prelude.ToHeaders CreateConnectorDefinition where
  toHeaders CreateConnectorDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateConnectorDefinition where
  toJSON CreateConnectorDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("InitialVersion" Prelude..=)
              Prelude.<$> initialVersion,
            ("tags" Prelude..=) Prelude.<$> tags
          ]
      )

instance Prelude.ToPath CreateConnectorDefinition where
  toPath =
    Prelude.const "/greengrass/definition/connectors"

instance Prelude.ToQuery CreateConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
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
-- Create a value of 'CreateConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createConnectorDefinitionResponse_httpStatus' - The response's http status code.
newCreateConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectorDefinitionResponse
newCreateConnectorDefinitionResponse pHttpStatus_ =
  CreateConnectorDefinitionResponse'
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
createConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_latestVersionArn = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_latestVersion = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the definition.
createConnectorDefinitionResponse_arn :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_arn = Lens.lens (\CreateConnectorDefinitionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the definition.
createConnectorDefinitionResponse_id :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_id = Lens.lens (\CreateConnectorDefinitionResponse' {id} -> id) (\s@CreateConnectorDefinitionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionResponse)

-- | The name of the definition.
createConnectorDefinitionResponse_name :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_name = Lens.lens (\CreateConnectorDefinitionResponse' {name} -> name) (\s@CreateConnectorDefinitionResponse' {} a -> s {name = a} :: CreateConnectorDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The response's http status code.
createConnectorDefinitionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionResponse Prelude.Int
createConnectorDefinitionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionResponse)

instance
  Prelude.NFData
    CreateConnectorDefinitionResponse
