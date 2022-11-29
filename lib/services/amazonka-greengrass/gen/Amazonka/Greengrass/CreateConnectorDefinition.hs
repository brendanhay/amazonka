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
-- Module      : Amazonka.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of
-- the connector definition now or use
-- \'\'CreateConnectorDefinitionVersion\'\' at a later time.
module Amazonka.Greengrass.CreateConnectorDefinition
  ( -- * Creating a Request
    CreateConnectorDefinition (..),
    newCreateConnectorDefinition,

    -- * Request Lenses
    createConnectorDefinition_tags,
    createConnectorDefinition_name,
    createConnectorDefinition_initialVersion,
    createConnectorDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateConnectorDefinitionResponse (..),
    newCreateConnectorDefinitionResponse,

    -- * Response Lenses
    createConnectorDefinitionResponse_lastUpdatedTimestamp,
    createConnectorDefinitionResponse_name,
    createConnectorDefinitionResponse_arn,
    createConnectorDefinitionResponse_latestVersion,
    createConnectorDefinitionResponse_id,
    createConnectorDefinitionResponse_creationTimestamp,
    createConnectorDefinitionResponse_latestVersionArn,
    createConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the connector definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the connector definition.
    initialVersion :: Prelude.Maybe ConnectorDefinitionVersion,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConnectorDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'name', 'createConnectorDefinition_name' - The name of the connector definition.
--
-- 'initialVersion', 'createConnectorDefinition_initialVersion' - Information about the initial version of the connector definition.
--
-- 'amznClientToken', 'createConnectorDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateConnectorDefinition ::
  CreateConnectorDefinition
newCreateConnectorDefinition =
  CreateConnectorDefinition'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | Tag(s) to add to the new resource.
createConnectorDefinition_tags :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConnectorDefinition_tags = Lens.lens (\CreateConnectorDefinition' {tags} -> tags) (\s@CreateConnectorDefinition' {} a -> s {tags = a} :: CreateConnectorDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the connector definition.
createConnectorDefinition_name :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe Prelude.Text)
createConnectorDefinition_name = Lens.lens (\CreateConnectorDefinition' {name} -> name) (\s@CreateConnectorDefinition' {} a -> s {name = a} :: CreateConnectorDefinition)

-- | Information about the initial version of the connector definition.
createConnectorDefinition_initialVersion :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe ConnectorDefinitionVersion)
createConnectorDefinition_initialVersion = Lens.lens (\CreateConnectorDefinition' {initialVersion} -> initialVersion) (\s@CreateConnectorDefinition' {} a -> s {initialVersion = a} :: CreateConnectorDefinition)

-- | A client token used to correlate requests and responses.
createConnectorDefinition_amznClientToken :: Lens.Lens' CreateConnectorDefinition (Prelude.Maybe Prelude.Text)
createConnectorDefinition_amznClientToken = Lens.lens (\CreateConnectorDefinition' {amznClientToken} -> amznClientToken) (\s@CreateConnectorDefinition' {} a -> s {amznClientToken = a} :: CreateConnectorDefinition)

instance Core.AWSRequest CreateConnectorDefinition where
  type
    AWSResponse CreateConnectorDefinition =
      CreateConnectorDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionResponse'
            Prelude.<$> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectorDefinition where
  hashWithSalt _salt CreateConnectorDefinition' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` amznClientToken

instance Prelude.NFData CreateConnectorDefinition where
  rnf CreateConnectorDefinition' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf initialVersion
      `Prelude.seq` Prelude.rnf amznClientToken

instance Core.ToHeaders CreateConnectorDefinition where
  toHeaders CreateConnectorDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateConnectorDefinition where
  toJSON CreateConnectorDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("Name" Core..=) Prelude.<$> name,
            ("InitialVersion" Core..=)
              Prelude.<$> initialVersion
          ]
      )

instance Core.ToPath CreateConnectorDefinition where
  toPath =
    Prelude.const "/greengrass/definition/connectors"

instance Core.ToQuery CreateConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'createConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'name', 'createConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'arn', 'createConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'latestVersion', 'createConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'createConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'creationTimestamp', 'createConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'httpStatus', 'createConnectorDefinitionResponse_httpStatus' - The response's http status code.
newCreateConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectorDefinitionResponse
newCreateConnectorDefinitionResponse pHttpStatus_ =
  CreateConnectorDefinitionResponse'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The name of the definition.
createConnectorDefinitionResponse_name :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_name = Lens.lens (\CreateConnectorDefinitionResponse' {name} -> name) (\s@CreateConnectorDefinitionResponse' {} a -> s {name = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the definition.
createConnectorDefinitionResponse_arn :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_arn = Lens.lens (\CreateConnectorDefinitionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_latestVersion = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: CreateConnectorDefinitionResponse)

-- | The ID of the definition.
createConnectorDefinitionResponse_id :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_id = Lens.lens (\CreateConnectorDefinitionResponse' {id} -> id) (\s@CreateConnectorDefinitionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' CreateConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionResponse_latestVersionArn = Lens.lens (\CreateConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateConnectorDefinitionResponse)

-- | The response's http status code.
createConnectorDefinitionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionResponse Prelude.Int
createConnectorDefinitionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionResponse)

instance
  Prelude.NFData
    CreateConnectorDefinitionResponse
  where
  rnf CreateConnectorDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
