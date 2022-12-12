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
-- Module      : Amazonka.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the
-- device definition now or use \'\'CreateDeviceDefinitionVersion\'\' at a
-- later time.
module Amazonka.Greengrass.CreateDeviceDefinition
  ( -- * Creating a Request
    CreateDeviceDefinition (..),
    newCreateDeviceDefinition,

    -- * Request Lenses
    createDeviceDefinition_amznClientToken,
    createDeviceDefinition_initialVersion,
    createDeviceDefinition_name,
    createDeviceDefinition_tags,

    -- * Destructuring the Response
    CreateDeviceDefinitionResponse (..),
    newCreateDeviceDefinitionResponse,

    -- * Response Lenses
    createDeviceDefinitionResponse_arn,
    createDeviceDefinitionResponse_creationTimestamp,
    createDeviceDefinitionResponse_id,
    createDeviceDefinitionResponse_lastUpdatedTimestamp,
    createDeviceDefinitionResponse_latestVersion,
    createDeviceDefinitionResponse_latestVersionArn,
    createDeviceDefinitionResponse_name,
    createDeviceDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the device definition.
    initialVersion :: Prelude.Maybe DeviceDefinitionVersion,
    -- | The name of the device definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createDeviceDefinition_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'initialVersion', 'createDeviceDefinition_initialVersion' - Information about the initial version of the device definition.
--
-- 'name', 'createDeviceDefinition_name' - The name of the device definition.
--
-- 'tags', 'createDeviceDefinition_tags' - Tag(s) to add to the new resource.
newCreateDeviceDefinition ::
  CreateDeviceDefinition
newCreateDeviceDefinition =
  CreateDeviceDefinition'
    { amznClientToken =
        Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A client token used to correlate requests and responses.
createDeviceDefinition_amznClientToken :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe Prelude.Text)
createDeviceDefinition_amznClientToken = Lens.lens (\CreateDeviceDefinition' {amznClientToken} -> amznClientToken) (\s@CreateDeviceDefinition' {} a -> s {amznClientToken = a} :: CreateDeviceDefinition)

-- | Information about the initial version of the device definition.
createDeviceDefinition_initialVersion :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe DeviceDefinitionVersion)
createDeviceDefinition_initialVersion = Lens.lens (\CreateDeviceDefinition' {initialVersion} -> initialVersion) (\s@CreateDeviceDefinition' {} a -> s {initialVersion = a} :: CreateDeviceDefinition)

-- | The name of the device definition.
createDeviceDefinition_name :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe Prelude.Text)
createDeviceDefinition_name = Lens.lens (\CreateDeviceDefinition' {name} -> name) (\s@CreateDeviceDefinition' {} a -> s {name = a} :: CreateDeviceDefinition)

-- | Tag(s) to add to the new resource.
createDeviceDefinition_tags :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeviceDefinition_tags = Lens.lens (\CreateDeviceDefinition' {tags} -> tags) (\s@CreateDeviceDefinition' {} a -> s {tags = a} :: CreateDeviceDefinition) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateDeviceDefinition where
  type
    AWSResponse CreateDeviceDefinition =
      CreateDeviceDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeviceDefinition where
  hashWithSalt _salt CreateDeviceDefinition' {..} =
    _salt `Prelude.hashWithSalt` amznClientToken
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateDeviceDefinition where
  rnf CreateDeviceDefinition' {..} =
    Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf initialVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateDeviceDefinition where
  toHeaders CreateDeviceDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateDeviceDefinition where
  toJSON CreateDeviceDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InitialVersion" Data..=)
              Prelude.<$> initialVersion,
            ("Name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateDeviceDefinition where
  toPath =
    Prelude.const "/greengrass/definition/devices"

instance Data.ToQuery CreateDeviceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
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
-- Create a value of 'CreateDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createDeviceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'createDeviceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'createDeviceDefinitionResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'createDeviceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'createDeviceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'createDeviceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'createDeviceDefinitionResponse_name' - The name of the definition.
--
-- 'httpStatus', 'createDeviceDefinitionResponse_httpStatus' - The response's http status code.
newCreateDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeviceDefinitionResponse
newCreateDeviceDefinitionResponse pHttpStatus_ =
  CreateDeviceDefinitionResponse'
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
createDeviceDefinitionResponse_arn :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_arn = Lens.lens (\CreateDeviceDefinitionResponse' {arn} -> arn) (\s@CreateDeviceDefinitionResponse' {} a -> s {arn = a} :: CreateDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createDeviceDefinitionResponse_creationTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_creationTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the definition.
createDeviceDefinitionResponse_id :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_id = Lens.lens (\CreateDeviceDefinitionResponse' {id} -> id) (\s@CreateDeviceDefinitionResponse' {} a -> s {id = a} :: CreateDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createDeviceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersion :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_latestVersion = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersion = a} :: CreateDeviceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersionArn :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_latestVersionArn = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateDeviceDefinitionResponse)

-- | The name of the definition.
createDeviceDefinitionResponse_name :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_name = Lens.lens (\CreateDeviceDefinitionResponse' {name} -> name) (\s@CreateDeviceDefinitionResponse' {} a -> s {name = a} :: CreateDeviceDefinitionResponse)

-- | The response's http status code.
createDeviceDefinitionResponse_httpStatus :: Lens.Lens' CreateDeviceDefinitionResponse Prelude.Int
createDeviceDefinitionResponse_httpStatus = Lens.lens (\CreateDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: CreateDeviceDefinitionResponse)

instance
  Prelude.NFData
    CreateDeviceDefinitionResponse
  where
  rnf CreateDeviceDefinitionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
