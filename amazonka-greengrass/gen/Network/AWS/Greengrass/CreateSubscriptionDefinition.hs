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
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version
-- of the subscription definition now or use
-- \'\'CreateSubscriptionDefinitionVersion\'\' at a later time.
module Network.AWS.Greengrass.CreateSubscriptionDefinition
  ( -- * Creating a Request
    CreateSubscriptionDefinition (..),
    newCreateSubscriptionDefinition,

    -- * Request Lenses
    createSubscriptionDefinition_name,
    createSubscriptionDefinition_initialVersion,
    createSubscriptionDefinition_tags,
    createSubscriptionDefinition_amznClientToken,

    -- * Destructuring the Response
    CreateSubscriptionDefinitionResponse (..),
    newCreateSubscriptionDefinitionResponse,

    -- * Response Lenses
    createSubscriptionDefinitionResponse_creationTimestamp,
    createSubscriptionDefinitionResponse_latestVersionArn,
    createSubscriptionDefinitionResponse_latestVersion,
    createSubscriptionDefinitionResponse_arn,
    createSubscriptionDefinitionResponse_id,
    createSubscriptionDefinitionResponse_name,
    createSubscriptionDefinitionResponse_lastUpdatedTimestamp,
    createSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { -- | The name of the subscription definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the initial version of the subscription definition.
    initialVersion :: Prelude.Maybe SubscriptionDefinitionVersion,
    -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'name', 'createSubscriptionDefinition_name' - The name of the subscription definition.
--
-- 'initialVersion', 'createSubscriptionDefinition_initialVersion' - Information about the initial version of the subscription definition.
--
-- 'tags', 'createSubscriptionDefinition_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'createSubscriptionDefinition_amznClientToken' - A client token used to correlate requests and responses.
newCreateSubscriptionDefinition ::
  CreateSubscriptionDefinition
newCreateSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { name =
        Prelude.Nothing,
      initialVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      amznClientToken = Prelude.Nothing
    }

-- | The name of the subscription definition.
createSubscriptionDefinition_name :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe Prelude.Text)
createSubscriptionDefinition_name = Lens.lens (\CreateSubscriptionDefinition' {name} -> name) (\s@CreateSubscriptionDefinition' {} a -> s {name = a} :: CreateSubscriptionDefinition)

-- | Information about the initial version of the subscription definition.
createSubscriptionDefinition_initialVersion :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe SubscriptionDefinitionVersion)
createSubscriptionDefinition_initialVersion = Lens.lens (\CreateSubscriptionDefinition' {initialVersion} -> initialVersion) (\s@CreateSubscriptionDefinition' {} a -> s {initialVersion = a} :: CreateSubscriptionDefinition)

-- | Tag(s) to add to the new resource.
createSubscriptionDefinition_tags :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSubscriptionDefinition_tags = Lens.lens (\CreateSubscriptionDefinition' {tags} -> tags) (\s@CreateSubscriptionDefinition' {} a -> s {tags = a} :: CreateSubscriptionDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createSubscriptionDefinition_amznClientToken :: Lens.Lens' CreateSubscriptionDefinition (Prelude.Maybe Prelude.Text)
createSubscriptionDefinition_amznClientToken = Lens.lens (\CreateSubscriptionDefinition' {amznClientToken} -> amznClientToken) (\s@CreateSubscriptionDefinition' {} a -> s {amznClientToken = a} :: CreateSubscriptionDefinition)

instance Core.AWSRequest CreateSubscriptionDefinition where
  type
    AWSResponse CreateSubscriptionDefinition =
      CreateSubscriptionDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriptionDefinition

instance Prelude.NFData CreateSubscriptionDefinition

instance Core.ToHeaders CreateSubscriptionDefinition where
  toHeaders CreateSubscriptionDefinition' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateSubscriptionDefinition where
  toJSON CreateSubscriptionDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("InitialVersion" Core..=)
              Prelude.<$> initialVersion,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateSubscriptionDefinition where
  toPath =
    Prelude.const
      "/greengrass/definition/subscriptions"

instance Core.ToQuery CreateSubscriptionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createSubscriptionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'createSubscriptionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'createSubscriptionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'createSubscriptionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'createSubscriptionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'createSubscriptionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'createSubscriptionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'createSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newCreateSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriptionDefinitionResponse
newCreateSubscriptionDefinitionResponse pHttpStatus_ =
  CreateSubscriptionDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
createSubscriptionDefinitionResponse_creationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_creationTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {creationTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersionArn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_latestVersionArn = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersionArn = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
createSubscriptionDefinitionResponse_latestVersion :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_latestVersion = Lens.lens (\CreateSubscriptionDefinitionResponse' {latestVersion} -> latestVersion) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {latestVersion = a} :: CreateSubscriptionDefinitionResponse)

-- | The ARN of the definition.
createSubscriptionDefinitionResponse_arn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_arn = Lens.lens (\CreateSubscriptionDefinitionResponse' {arn} -> arn) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {arn = a} :: CreateSubscriptionDefinitionResponse)

-- | The ID of the definition.
createSubscriptionDefinitionResponse_id :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_id = Lens.lens (\CreateSubscriptionDefinitionResponse' {id} -> id) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {id = a} :: CreateSubscriptionDefinitionResponse)

-- | The name of the definition.
createSubscriptionDefinitionResponse_name :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_name = Lens.lens (\CreateSubscriptionDefinitionResponse' {name} -> name) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {name = a} :: CreateSubscriptionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
createSubscriptionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Prelude.Maybe Prelude.Text)
createSubscriptionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\CreateSubscriptionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: CreateSubscriptionDefinitionResponse)

-- | The response's http status code.
createSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' CreateSubscriptionDefinitionResponse Prelude.Int
createSubscriptionDefinitionResponse_httpStatus = Lens.lens (\CreateSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionDefinitionResponse)

instance
  Prelude.NFData
    CreateSubscriptionDefinitionResponse
