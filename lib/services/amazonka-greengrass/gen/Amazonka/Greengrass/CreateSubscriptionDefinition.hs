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
-- Module      : Amazonka.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version
-- of the subscription definition now or use
-- \'\'CreateSubscriptionDefinitionVersion\'\' at a later time.
module Amazonka.Greengrass.CreateSubscriptionDefinition
  ( -- * Creating a Request
    CreateSubscriptionDefinition (..),
    newCreateSubscriptionDefinition,

    -- * Request Lenses
    createSubscriptionDefinition_tags,
    createSubscriptionDefinition_name,
    createSubscriptionDefinition_initialVersion,
    createSubscriptionDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateSubscriptionDefinitionResponse (..),
    newCreateSubscriptionDefinitionResponse,

    -- * Response Lenses
    createSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    createSubscriptionDefinitionResponse_name,
    createSubscriptionDefinitionResponse_arn,
    createSubscriptionDefinitionResponse_latestVersion,
    createSubscriptionDefinitionResponse_id,
    createSubscriptionDefinitionResponse_creationTimestamp,
    createSubscriptionDefinitionResponse_latestVersionArn,
    createSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the subscription definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the subscription definition.
    initialVersion :: Prelude.Maybe SubscriptionDefinitionVersion,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSubscriptionDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'name', 'createSubscriptionDefinition_name' - The name of the subscription definition.
--
-- 'initialVersion', 'createSubscriptionDefinition_initialVersion' - Information about the initial version of the subscription definition.
--
-- 'amznClientToken', 'createSubscriptionDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateSubscriptionDefinition ::
  CreateSubscriptionDefinition
newCreateSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | Tag(s) to add to the new resource.
createSubscriptionDefinition_tags :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSubscriptionDefinition_tags = Lens.lens (\CreateSubscriptionDefinition' {tags} -> tags) (\s@CreateSubscriptionDefinition' {} a -> s {tags = a} :: CreateSubscriptionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the subscription definition.
createSubscriptionDefinition_name :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe Prelude.Text)
createSubscriptionDefinition_name = Lens.lens (\CreateSubscriptionDefinition' {name} -> name) (\s@CreateSubscriptionDefinition' {} a -> s {name = a} :: CreateSubscriptionDefinition)

-- | Information about the initial version of the subscription definition.
createSubscriptionDefinition_initialVersion :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe SubscriptionDefinitionVersion)
createSubscriptionDefinition_initialVersion = Lens.lens (\CreateSubscriptionDefinition' {initialVersion} -> initialVersion) (\s@CreateSubscriptionDefinition' {} a -> s {initialVersion = a} :: CreateSubscriptionDefinition)

-- | A client token used to correlate requests and responses.
createSubscriptionDefinition_amznClientToken :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe Prelude.Text)
createSubscriptionDefinition_amznClientToken = Lens.lens (\CreateSubscriptionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinition' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinition)

instance Core.AWSRequest CreateSubscriptionDefinition where
  type
    AWSResponse CreateSubscriptionDefinition =
      CreateSubscriptionDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionResponse'
            Prelude.<$> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriptionDefinition
  where
  hashWithSalt _salt CreateSubscriptionDefinition' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` initialVersion
      `Prelude.hashWithSalt` amznClientToken

instance Prelude.NFData CreateSubscriptionDefinition where
  rnf CreateSubscriptionDefinition' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf initialVersion
      `Prelude.seq` Prelude.rnf amznClientToken

instance Data.ToHeaders CreateSubscriptionDefinition where
  toHeaders CreateSubscriptionDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateSubscriptionDefinition where
  toJSON CreateSubscriptionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("Name" Data..=) Prelude.<$> name,
            ("InitialVersion" Data..=)
              Prelude.<$> initialVersion
          ]
      )

instance Data.ToPath CreateSubscriptionDefinition where
  toPath =
    Prelude.const
      "/greengrass/definition/subscriptions"

instance Data.ToQuery CreateSubscriptionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
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
-- Create a value of 'CreateSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'createSubscriptionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'name', 'createSubscriptionDefinitionResponse_name' - The name of the definition.
--
-- 'arn', 'createSubscriptionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'latestVersion', 'createSubscriptionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'createSubscriptionDefinitionResponse_id' - The ID of the definition.
--
-- 'creationTimestamp', 'createSubscriptionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createSubscriptionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'httpStatus', 'createSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newCreateSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriptionDefinitionResponse
newCreateSubscriptionDefinitionResponse pHttpStatus_ =
  CreateSubscriptionDefinitionResponse'
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
createSubscriptionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The name of the definition.
createSubscriptionDefinitionResponse_name :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_name = Lens.lens (\CreateSubscriptionDefinitionResponse' {name} -> name) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {name = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the definition.
createSubscriptionDefinitionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersion :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_latestVersion = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the definition.
createSubscriptionDefinitionResponse_id :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_id = Lens.lens (\CreateSubscriptionDefinitionResponse' {id} -> id) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {id = a} :: CreateSubscriptionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createSubscriptionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_latestVersionArn = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateSubscriptionDefinitionResponse)

-- | The response's http status code.
createSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' CreateSubscriptionDefinitionResponse Prelude.Int
createSubscriptionDefinitionResponse_httpStatus = Lens.lens (\CreateSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionDefinitionResponse)

instance
  Prelude.NFData
    CreateSubscriptionDefinitionResponse
  where
  rnf CreateSubscriptionDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
