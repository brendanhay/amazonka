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
-- Module      : Amazonka.ChimeSDKIdentity.CreateAppInstanceBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bot under an Amazon Chime @AppInstance@. The request consists
-- of a unique @Configuration@ and @Name@ for that bot.
module Amazonka.ChimeSDKIdentity.CreateAppInstanceBot
  ( -- * Creating a Request
    CreateAppInstanceBot (..),
    newCreateAppInstanceBot,

    -- * Request Lenses
    createAppInstanceBot_metadata,
    createAppInstanceBot_name,
    createAppInstanceBot_tags,
    createAppInstanceBot_appInstanceArn,
    createAppInstanceBot_clientRequestToken,
    createAppInstanceBot_configuration,

    -- * Destructuring the Response
    CreateAppInstanceBotResponse (..),
    newCreateAppInstanceBotResponse,

    -- * Response Lenses
    createAppInstanceBotResponse_appInstanceBotArn,
    createAppInstanceBotResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppInstanceBot' smart constructor.
data CreateAppInstanceBot = CreateAppInstanceBot'
  { -- | The request metadata. Limited to a 1KB string in UTF-8.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The user\'s name.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The tags assigned to the @AppInstanceBot@.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ARN of the @AppInstance@ request.
    appInstanceArn :: Prelude.Text,
    -- | The unique ID for the client making the request. Use different tokens
    -- for different @AppInstanceBots@.
    clientRequestToken :: Prelude.Text,
    -- | Configuration information about the Amazon Lex V2 V2 bot.
    configuration :: Configuration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'createAppInstanceBot_metadata' - The request metadata. Limited to a 1KB string in UTF-8.
--
-- 'name', 'createAppInstanceBot_name' - The user\'s name.
--
-- 'tags', 'createAppInstanceBot_tags' - The tags assigned to the @AppInstanceBot@.
--
-- 'appInstanceArn', 'createAppInstanceBot_appInstanceArn' - The ARN of the @AppInstance@ request.
--
-- 'clientRequestToken', 'createAppInstanceBot_clientRequestToken' - The unique ID for the client making the request. Use different tokens
-- for different @AppInstanceBots@.
--
-- 'configuration', 'createAppInstanceBot_configuration' - Configuration information about the Amazon Lex V2 V2 bot.
newCreateAppInstanceBot ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'configuration'
  Configuration ->
  CreateAppInstanceBot
newCreateAppInstanceBot
  pAppInstanceArn_
  pClientRequestToken_
  pConfiguration_ =
    CreateAppInstanceBot'
      { metadata = Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        appInstanceArn = pAppInstanceArn_,
        clientRequestToken = pClientRequestToken_,
        configuration = pConfiguration_
      }

-- | The request metadata. Limited to a 1KB string in UTF-8.
createAppInstanceBot_metadata :: Lens.Lens' CreateAppInstanceBot (Prelude.Maybe Prelude.Text)
createAppInstanceBot_metadata = Lens.lens (\CreateAppInstanceBot' {metadata} -> metadata) (\s@CreateAppInstanceBot' {} a -> s {metadata = a} :: CreateAppInstanceBot) Prelude.. Lens.mapping Data._Sensitive

-- | The user\'s name.
createAppInstanceBot_name :: Lens.Lens' CreateAppInstanceBot (Prelude.Maybe Prelude.Text)
createAppInstanceBot_name = Lens.lens (\CreateAppInstanceBot' {name} -> name) (\s@CreateAppInstanceBot' {} a -> s {name = a} :: CreateAppInstanceBot) Prelude.. Lens.mapping Data._Sensitive

-- | The tags assigned to the @AppInstanceBot@.
createAppInstanceBot_tags :: Lens.Lens' CreateAppInstanceBot (Prelude.Maybe (Prelude.NonEmpty Tag))
createAppInstanceBot_tags = Lens.lens (\CreateAppInstanceBot' {tags} -> tags) (\s@CreateAppInstanceBot' {} a -> s {tags = a} :: CreateAppInstanceBot) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the @AppInstance@ request.
createAppInstanceBot_appInstanceArn :: Lens.Lens' CreateAppInstanceBot Prelude.Text
createAppInstanceBot_appInstanceArn = Lens.lens (\CreateAppInstanceBot' {appInstanceArn} -> appInstanceArn) (\s@CreateAppInstanceBot' {} a -> s {appInstanceArn = a} :: CreateAppInstanceBot)

-- | The unique ID for the client making the request. Use different tokens
-- for different @AppInstanceBots@.
createAppInstanceBot_clientRequestToken :: Lens.Lens' CreateAppInstanceBot Prelude.Text
createAppInstanceBot_clientRequestToken = Lens.lens (\CreateAppInstanceBot' {clientRequestToken} -> clientRequestToken) (\s@CreateAppInstanceBot' {} a -> s {clientRequestToken = a} :: CreateAppInstanceBot)

-- | Configuration information about the Amazon Lex V2 V2 bot.
createAppInstanceBot_configuration :: Lens.Lens' CreateAppInstanceBot Configuration
createAppInstanceBot_configuration = Lens.lens (\CreateAppInstanceBot' {configuration} -> configuration) (\s@CreateAppInstanceBot' {} a -> s {configuration = a} :: CreateAppInstanceBot)

instance Core.AWSRequest CreateAppInstanceBot where
  type
    AWSResponse CreateAppInstanceBot =
      CreateAppInstanceBotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppInstanceBotResponse'
            Prelude.<$> (x Data..?> "AppInstanceBotArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppInstanceBot where
  hashWithSalt _salt CreateAppInstanceBot' {..} =
    _salt
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` appInstanceArn
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData CreateAppInstanceBot where
  rnf CreateAppInstanceBot' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToHeaders CreateAppInstanceBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAppInstanceBot where
  toJSON CreateAppInstanceBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Metadata" Data..=) Prelude.<$> metadata,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("AppInstanceArn" Data..= appInstanceArn),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )

instance Data.ToPath CreateAppInstanceBot where
  toPath = Prelude.const "/app-instance-bots"

instance Data.ToQuery CreateAppInstanceBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppInstanceBotResponse' smart constructor.
data CreateAppInstanceBotResponse = CreateAppInstanceBotResponse'
  { -- | The ARN of the @AppinstanceBot@.
    appInstanceBotArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'createAppInstanceBotResponse_appInstanceBotArn' - The ARN of the @AppinstanceBot@.
--
-- 'httpStatus', 'createAppInstanceBotResponse_httpStatus' - The response's http status code.
newCreateAppInstanceBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppInstanceBotResponse
newCreateAppInstanceBotResponse pHttpStatus_ =
  CreateAppInstanceBotResponse'
    { appInstanceBotArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the @AppinstanceBot@.
createAppInstanceBotResponse_appInstanceBotArn :: Lens.Lens' CreateAppInstanceBotResponse (Prelude.Maybe Prelude.Text)
createAppInstanceBotResponse_appInstanceBotArn = Lens.lens (\CreateAppInstanceBotResponse' {appInstanceBotArn} -> appInstanceBotArn) (\s@CreateAppInstanceBotResponse' {} a -> s {appInstanceBotArn = a} :: CreateAppInstanceBotResponse)

-- | The response's http status code.
createAppInstanceBotResponse_httpStatus :: Lens.Lens' CreateAppInstanceBotResponse Prelude.Int
createAppInstanceBotResponse_httpStatus = Lens.lens (\CreateAppInstanceBotResponse' {httpStatus} -> httpStatus) (\s@CreateAppInstanceBotResponse' {} a -> s {httpStatus = a} :: CreateAppInstanceBotResponse)

instance Prelude.NFData CreateAppInstanceBotResponse where
  rnf CreateAppInstanceBotResponse' {..} =
    Prelude.rnf appInstanceBotArn
      `Prelude.seq` Prelude.rnf httpStatus
