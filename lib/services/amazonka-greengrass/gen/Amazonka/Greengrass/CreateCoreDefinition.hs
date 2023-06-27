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
-- Module      : Amazonka.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the
-- core definition now or use \'\'CreateCoreDefinitionVersion\'\' at a
-- later time. Greengrass groups must each contain exactly one Greengrass
-- core.
module Amazonka.Greengrass.CreateCoreDefinition
  ( -- * Creating a Request
    CreateCoreDefinition (..),
    newCreateCoreDefinition,

    -- * Request Lenses
    createCoreDefinition_amznClientToken,
    createCoreDefinition_initialVersion,
    createCoreDefinition_name,
    createCoreDefinition_tags,

    -- * Destructuring the Response
    CreateCoreDefinitionResponse (..),
    newCreateCoreDefinitionResponse,

    -- * Response Lenses
    createCoreDefinitionResponse_arn,
    createCoreDefinitionResponse_creationTimestamp,
    createCoreDefinitionResponse_id,
    createCoreDefinitionResponse_lastUpdatedTimestamp,
    createCoreDefinitionResponse_latestVersion,
    createCoreDefinitionResponse_latestVersionArn,
    createCoreDefinitionResponse_name,
    createCoreDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Information needed to create a core definition.
--
-- /See:/ 'newCreateCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the core definition.
    initialVersion :: Prelude.Maybe CoreDefinitionVersion,
    -- | The name of the core definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createCoreDefinition_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'initialVersion', 'createCoreDefinition_initialVersion' - Information about the initial version of the core definition.
--
-- 'name', 'createCoreDefinition_name' - The name of the core definition.
--
-- 'tags', 'createCoreDefinition_tags' - Tag(s) to add to the new resource.
newCreateCoreDefinition ::
  CreateCoreDefinition
newCreateCoreDefinition =
  CreateCoreDefinition'
    { amznClientToken =
        Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A client token used to correlate requests and responses.
createCoreDefinition_amznClientToken :: Lens.Lens' CreateCoreDefinition (Prelude.Maybe Prelude.Text)
createCoreDefinition_amznClientToken = Lens.lens (\CreateCoreDefinition' {amznClientToken} -> amznClientToken) (\s@CreateCoreDefinition' {} a -> s {amznClientToken = a} :: CreateCoreDefinition)

-- | Information about the initial version of the core definition.
createCoreDefinition_initialVersion :: Lens.Lens' CreateCoreDefinition (Prelude.Maybe CoreDefinitionVersion)
createCoreDefinition_initialVersion = Lens.lens (\CreateCoreDefinition' {initialVersion} -> initialVersion) (\s@CreateCoreDefinition' {} a -> s {initialVersion = a} :: CreateCoreDefinition)

-- | The name of the core definition.
createCoreDefinition_name :: Lens.Lens' CreateCoreDefinition (Prelude.Maybe Prelude.Text)
createCoreDefinition_name = Lens.lens (\CreateCoreDefinition' {name} -> name) (\s@CreateCoreDefinition' {} a -> s {name = a} :: CreateCoreDefinition)

-- | Tag(s) to add to the new resource.
createCoreDefinition_tags :: Lens.Lens' CreateCoreDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCoreDefinition_tags = Lens.lens (\CreateCoreDefinition' {tags} -> tags) (\s@CreateCoreDefinition' {} a -> s {tags = a} :: CreateCoreDefinition) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateCoreDefinition where
  type
    AWSResponse CreateCoreDefinition =
      CreateCoreDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCoreDefinition where
  hashWithSalt _salt CreateCoreDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` amznClientToken
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateCoreDefinition where
  rnf CreateCoreDefinition' {..} =
    Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf initialVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateCoreDefinition where
  toHeaders CreateCoreDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateCoreDefinition where
  toJSON CreateCoreDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InitialVersion" Data..=)
              Prelude.<$> initialVersion,
            ("Name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateCoreDefinition where
  toPath = Prelude.const "/greengrass/definition/cores"

instance Data.ToQuery CreateCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
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
-- Create a value of 'CreateCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createCoreDefinitionResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'createCoreDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'createCoreDefinitionResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'createCoreDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'createCoreDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'createCoreDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'createCoreDefinitionResponse_name' - The name of the definition.
--
-- 'httpStatus', 'createCoreDefinitionResponse_httpStatus' - The response's http status code.
newCreateCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCoreDefinitionResponse
newCreateCoreDefinitionResponse pHttpStatus_ =
  CreateCoreDefinitionResponse'
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
createCoreDefinitionResponse_arn :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_arn = Lens.lens (\CreateCoreDefinitionResponse' {arn} -> arn) (\s@CreateCoreDefinitionResponse' {} a -> s {arn = a} :: CreateCoreDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createCoreDefinitionResponse_creationTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_creationTimestamp = Lens.lens (\CreateCoreDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateCoreDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateCoreDefinitionResponse)

-- | The ID of the definition.
createCoreDefinitionResponse_id :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_id = Lens.lens (\CreateCoreDefinitionResponse' {id} -> id) (\s@CreateCoreDefinitionResponse' {} a -> s {id = a} :: CreateCoreDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createCoreDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateCoreDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateCoreDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateCoreDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createCoreDefinitionResponse_latestVersion :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_latestVersion = Lens.lens (\CreateCoreDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateCoreDefinitionResponse' {} a -> s {latestVersion = a} :: CreateCoreDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createCoreDefinitionResponse_latestVersionArn :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_latestVersionArn = Lens.lens (\CreateCoreDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateCoreDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateCoreDefinitionResponse)

-- | The name of the definition.
createCoreDefinitionResponse_name :: Lens.Lens' CreateCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
createCoreDefinitionResponse_name = Lens.lens (\CreateCoreDefinitionResponse' {name} -> name) (\s@CreateCoreDefinitionResponse' {} a -> s {name = a} :: CreateCoreDefinitionResponse)

-- | The response's http status code.
createCoreDefinitionResponse_httpStatus :: Lens.Lens' CreateCoreDefinitionResponse Prelude.Int
createCoreDefinitionResponse_httpStatus = Lens.lens (\CreateCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCoreDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCoreDefinitionResponse)

instance Prelude.NFData CreateCoreDefinitionResponse where
  rnf CreateCoreDefinitionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
