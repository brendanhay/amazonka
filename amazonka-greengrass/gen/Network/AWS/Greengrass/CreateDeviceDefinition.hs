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
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the
-- device definition now or use \'\'CreateDeviceDefinitionVersion\'\' at a
-- later time.
module Network.AWS.Greengrass.CreateDeviceDefinition
  ( -- * Creating a Request
    CreateDeviceDefinition (..),
    newCreateDeviceDefinition,

    -- * Request Lenses
    createDeviceDefinition_name,
    createDeviceDefinition_initialVersion,
    createDeviceDefinition_tags,
    createDeviceDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateDeviceDefinitionResponse (..),
    newCreateDeviceDefinitionResponse,

    -- * Response Lenses
    createDeviceDefinitionResponse_creationTimestamp,
    createDeviceDefinitionResponse_latestVersionArn,
    createDeviceDefinitionResponse_latestVersion,
    createDeviceDefinitionResponse_arn,
    createDeviceDefinitionResponse_id,
    createDeviceDefinitionResponse_name,
    createDeviceDefinitionResponse_lastUpdatedTimestamp,
    createDeviceDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { -- | The name of the device definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the device definition.
    initialVersion :: Prelude.Maybe DeviceDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDeviceDefinition_name' - The name of the device definition.
--
-- 'initialVersion', 'createDeviceDefinition_initialVersion' - Information about the initial version of the device definition.
--
-- 'tags', 'createDeviceDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createDeviceDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateDeviceDefinition ::
  CreateDeviceDefinition
newCreateDeviceDefinition =
  CreateDeviceDefinition'
    { name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the device definition.
createDeviceDefinition_name :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe Prelude.Text)
createDeviceDefinition_name = Lens.lens (\CreateDeviceDefinition' {name} -> name) (\s@CreateDeviceDefinition' {} a -> s {name = a} :: CreateDeviceDefinition)

-- | Information about the initial version of the device definition.
createDeviceDefinition_initialVersion :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe DeviceDefinitionVersion)
createDeviceDefinition_initialVersion = Lens.lens (\CreateDeviceDefinition' {initialVersion} -> initialVersion) (\s@CreateDeviceDefinition' {} a -> s {initialVersion = a} :: CreateDeviceDefinition)

-- | Tag(s) to add to the new resource.
createDeviceDefinition_tags :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeviceDefinition_tags = Lens.lens (\CreateDeviceDefinition' {tags} -> tags) (\s@CreateDeviceDefinition' {} a -> s {tags = a} :: CreateDeviceDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createDeviceDefinition_amznClientToken :: Lens.Lens' CreateDeviceDefinition (Prelude.Maybe Prelude.Text)
createDeviceDefinition_amznClientToken = Lens.lens (\CreateDeviceDefinition' {amznClientToken} -> amznClientToken) (\s@CreateDeviceDefinition' {} a -> s {amznClientToken = a} :: CreateDeviceDefinition)

instance Prelude.AWSRequest CreateDeviceDefinition where
  type
    Rs CreateDeviceDefinition =
      CreateDeviceDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceDefinitionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "LatestVersionArn")
            Prelude.<*> (x Prelude..?> "LatestVersion")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeviceDefinition

instance Prelude.NFData CreateDeviceDefinition

instance Prelude.ToHeaders CreateDeviceDefinition where
  toHeaders CreateDeviceDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateDeviceDefinition where
  toJSON CreateDeviceDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("InitialVersion" Prelude..=)
              Prelude.<$> initialVersion,
            ("tags" Prelude..=) Prelude.<$> tags
          ]
      )

instance Prelude.ToPath CreateDeviceDefinition where
  toPath =
    Prelude.const "/greengrass/definition/devices"

instance Prelude.ToQuery CreateDeviceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
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
-- Create a value of 'CreateDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createDeviceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createDeviceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createDeviceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createDeviceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createDeviceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createDeviceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createDeviceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createDeviceDefinitionResponse_httpStatus' - The response's http status code.
newCreateDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeviceDefinitionResponse
newCreateDeviceDefinitionResponse pHttpStatus_ =
  CreateDeviceDefinitionResponse'
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
createDeviceDefinitionResponse_creationTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_creationTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersionArn :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_latestVersionArn = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createDeviceDefinitionResponse_latestVersion :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_latestVersion = Lens.lens (\CreateDeviceDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateDeviceDefinitionResponse' {} a -> s {latestVersion = a} :: CreateDeviceDefinitionResponse)

-- | The ARN of the definition.
createDeviceDefinitionResponse_arn :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_arn = Lens.lens (\CreateDeviceDefinitionResponse' {arn} -> arn) (\s@CreateDeviceDefinitionResponse' {} a -> s {arn = a} :: CreateDeviceDefinitionResponse)

-- | The ID of the definition.
createDeviceDefinitionResponse_id :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_id = Lens.lens (\CreateDeviceDefinitionResponse' {id} -> id) (\s@CreateDeviceDefinitionResponse' {} a -> s {id = a} :: CreateDeviceDefinitionResponse)

-- | The name of the definition.
createDeviceDefinitionResponse_name :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_name = Lens.lens (\CreateDeviceDefinitionResponse' {name} -> name) (\s@CreateDeviceDefinitionResponse' {} a -> s {name = a} :: CreateDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createDeviceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
createDeviceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateDeviceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateDeviceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateDeviceDefinitionResponse)

-- | The response's http status code.
createDeviceDefinitionResponse_httpStatus :: Lens.Lens' CreateDeviceDefinitionResponse Prelude.Int
createDeviceDefinitionResponse_httpStatus = Lens.lens (\CreateDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: CreateDeviceDefinitionResponse)

instance
  Prelude.NFData
    CreateDeviceDefinitionResponse
