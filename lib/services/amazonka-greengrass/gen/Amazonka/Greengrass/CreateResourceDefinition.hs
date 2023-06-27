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
-- Module      : Amazonka.Greengrass.CreateResourceDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource definition which contains a list of resources to be
-- used in a group. You can create an initial version of the definition by
-- providing a list of resources now, or use
-- \'\'CreateResourceDefinitionVersion\'\' later.
module Amazonka.Greengrass.CreateResourceDefinition
  ( -- * Creating a Request
    CreateResourceDefinition (..),
    newCreateResourceDefinition,

    -- * Request Lenses
    createResourceDefinition_amznClientToken,
    createResourceDefinition_initialVersion,
    createResourceDefinition_name,
    createResourceDefinition_tags,

    -- * Destructuring the Response
    CreateResourceDefinitionResponse (..),
    newCreateResourceDefinitionResponse,

    -- * Response Lenses
    createResourceDefinitionResponse_arn,
    createResourceDefinitionResponse_creationTimestamp,
    createResourceDefinitionResponse_id,
    createResourceDefinitionResponse_lastUpdatedTimestamp,
    createResourceDefinitionResponse_latestVersion,
    createResourceDefinitionResponse_latestVersionArn,
    createResourceDefinitionResponse_name,
    createResourceDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceDefinition' smart constructor.
data CreateResourceDefinition = CreateResourceDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the resource definition.
    initialVersion :: Prelude.Maybe ResourceDefinitionVersion,
    -- | The name of the resource definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createResourceDefinition_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'initialVersion', 'createResourceDefinition_initialVersion' - Information about the initial version of the resource definition.
--
-- 'name', 'createResourceDefinition_name' - The name of the resource definition.
--
-- 'tags', 'createResourceDefinition_tags' - Tag(s) to add to the new resource.
newCreateResourceDefinition ::
  CreateResourceDefinition
newCreateResourceDefinition =
  CreateResourceDefinition'
    { amznClientToken =
        Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A client token used to correlate requests and responses.
createResourceDefinition_amznClientToken :: Lens.Lens' CreateResourceDefinition (Prelude.Maybe Prelude.Text)
createResourceDefinition_amznClientToken = Lens.lens (\CreateResourceDefinition' {amznClientToken} -> amznClientToken) (\s@CreateResourceDefinition' {} a -> s {amznClientToken = a} :: CreateResourceDefinition)

-- | Information about the initial version of the resource definition.
createResourceDefinition_initialVersion :: Lens.Lens' CreateResourceDefinition (Prelude.Maybe ResourceDefinitionVersion)
createResourceDefinition_initialVersion = Lens.lens (\CreateResourceDefinition' {initialVersion} -> initialVersion) (\s@CreateResourceDefinition' {} a -> s {initialVersion = a} :: CreateResourceDefinition)

-- | The name of the resource definition.
createResourceDefinition_name :: Lens.Lens' CreateResourceDefinition (Prelude.Maybe Prelude.Text)
createResourceDefinition_name = Lens.lens (\CreateResourceDefinition' {name} -> name) (\s@CreateResourceDefinition' {} a -> s {name = a} :: CreateResourceDefinition)

-- | Tag(s) to add to the new resource.
createResourceDefinition_tags :: Lens.Lens' CreateResourceDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResourceDefinition_tags = Lens.lens (\CreateResourceDefinition' {tags} -> tags) (\s@CreateResourceDefinition' {} a -> s {tags = a} :: CreateResourceDefinition) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateResourceDefinition where
  type
    AWSResponse CreateResourceDefinition =
      CreateResourceDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResourceDefinition where
  hashWithSalt _salt CreateResourceDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` amznClientToken
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateResourceDefinition where
  rnf CreateResourceDefinition' {..} =
    Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf initialVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateResourceDefinition where
  toHeaders CreateResourceDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateResourceDefinition where
  toJSON CreateResourceDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InitialVersion" Data..=)
              Prelude.<$> initialVersion,
            ("Name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateResourceDefinition where
  toPath =
    Prelude.const "/greengrass/definition/resources"

instance Data.ToQuery CreateResourceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceDefinitionResponse' smart constructor.
data CreateResourceDefinitionResponse = CreateResourceDefinitionResponse'
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
-- Create a value of 'CreateResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createResourceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'createResourceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'createResourceDefinitionResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'createResourceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'createResourceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'createResourceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'createResourceDefinitionResponse_name' - The name of the definition.
--
-- 'httpStatus', 'createResourceDefinitionResponse_httpStatus' - The response's http status code.
newCreateResourceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceDefinitionResponse
newCreateResourceDefinitionResponse pHttpStatus_ =
  CreateResourceDefinitionResponse'
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
createResourceDefinitionResponse_arn :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_arn = Lens.lens (\CreateResourceDefinitionResponse' {arn} -> arn) (\s@CreateResourceDefinitionResponse' {} a -> s {arn = a} :: CreateResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createResourceDefinitionResponse_creationTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_creationTimestamp = Lens.lens (\CreateResourceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateResourceDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateResourceDefinitionResponse)

-- | The ID of the definition.
createResourceDefinitionResponse_id :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_id = Lens.lens (\CreateResourceDefinitionResponse' {id} -> id) (\s@CreateResourceDefinitionResponse' {} a -> s {id = a} :: CreateResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createResourceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateResourceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateResourceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateResourceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createResourceDefinitionResponse_latestVersion :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_latestVersion = Lens.lens (\CreateResourceDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateResourceDefinitionResponse' {} a -> s {latestVersion = a} :: CreateResourceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createResourceDefinitionResponse_latestVersionArn :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_latestVersionArn = Lens.lens (\CreateResourceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateResourceDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateResourceDefinitionResponse)

-- | The name of the definition.
createResourceDefinitionResponse_name :: Lens.Lens' CreateResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionResponse_name = Lens.lens (\CreateResourceDefinitionResponse' {name} -> name) (\s@CreateResourceDefinitionResponse' {} a -> s {name = a} :: CreateResourceDefinitionResponse)

-- | The response's http status code.
createResourceDefinitionResponse_httpStatus :: Lens.Lens' CreateResourceDefinitionResponse Prelude.Int
createResourceDefinitionResponse_httpStatus = Lens.lens (\CreateResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateResourceDefinitionResponse' {} a -> s {httpStatus = a} :: CreateResourceDefinitionResponse)

instance
  Prelude.NFData
    CreateResourceDefinitionResponse
  where
  rnf CreateResourceDefinitionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
