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
-- Module      : Amazonka.ChimeSDKIdentity.CreateAppInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Chime SDK messaging @AppInstance@ under an AWS
-- account. Only SDK messaging customers use this API. @CreateAppInstance@
-- supports idempotency behavior as described in the AWS API Standard.
--
-- identity
module Amazonka.ChimeSDKIdentity.CreateAppInstance
  ( -- * Creating a Request
    CreateAppInstance (..),
    newCreateAppInstance,

    -- * Request Lenses
    createAppInstance_metadata,
    createAppInstance_tags,
    createAppInstance_name,
    createAppInstance_clientRequestToken,

    -- * Destructuring the Response
    CreateAppInstanceResponse (..),
    newCreateAppInstanceResponse,

    -- * Response Lenses
    createAppInstanceResponse_appInstanceArn,
    createAppInstanceResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppInstance' smart constructor.
data CreateAppInstance = CreateAppInstance'
  { -- | The metadata of the @AppInstance@. Limited to a 1KB string in UTF-8.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Tags assigned to the @AppInstanceUser@.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the @AppInstance@.
    name :: Data.Sensitive Prelude.Text,
    -- | The @ClientRequestToken@ of the @AppInstance@.
    clientRequestToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'createAppInstance_metadata' - The metadata of the @AppInstance@. Limited to a 1KB string in UTF-8.
--
-- 'tags', 'createAppInstance_tags' - Tags assigned to the @AppInstanceUser@.
--
-- 'name', 'createAppInstance_name' - The name of the @AppInstance@.
--
-- 'clientRequestToken', 'createAppInstance_clientRequestToken' - The @ClientRequestToken@ of the @AppInstance@.
newCreateAppInstance ::
  -- | 'name'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateAppInstance
newCreateAppInstance pName_ pClientRequestToken_ =
  CreateAppInstance'
    { metadata = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = Data._Sensitive Lens.# pName_,
      clientRequestToken =
        Data._Sensitive Lens.# pClientRequestToken_
    }

-- | The metadata of the @AppInstance@. Limited to a 1KB string in UTF-8.
createAppInstance_metadata :: Lens.Lens' CreateAppInstance (Prelude.Maybe Prelude.Text)
createAppInstance_metadata = Lens.lens (\CreateAppInstance' {metadata} -> metadata) (\s@CreateAppInstance' {} a -> s {metadata = a} :: CreateAppInstance) Prelude.. Lens.mapping Data._Sensitive

-- | Tags assigned to the @AppInstanceUser@.
createAppInstance_tags :: Lens.Lens' CreateAppInstance (Prelude.Maybe (Prelude.NonEmpty Tag))
createAppInstance_tags = Lens.lens (\CreateAppInstance' {tags} -> tags) (\s@CreateAppInstance' {} a -> s {tags = a} :: CreateAppInstance) Prelude.. Lens.mapping Lens.coerced

-- | The name of the @AppInstance@.
createAppInstance_name :: Lens.Lens' CreateAppInstance Prelude.Text
createAppInstance_name = Lens.lens (\CreateAppInstance' {name} -> name) (\s@CreateAppInstance' {} a -> s {name = a} :: CreateAppInstance) Prelude.. Data._Sensitive

-- | The @ClientRequestToken@ of the @AppInstance@.
createAppInstance_clientRequestToken :: Lens.Lens' CreateAppInstance Prelude.Text
createAppInstance_clientRequestToken = Lens.lens (\CreateAppInstance' {clientRequestToken} -> clientRequestToken) (\s@CreateAppInstance' {} a -> s {clientRequestToken = a} :: CreateAppInstance) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateAppInstance where
  type
    AWSResponse CreateAppInstance =
      CreateAppInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppInstanceResponse'
            Prelude.<$> (x Data..?> "AppInstanceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppInstance where
  hashWithSalt _salt CreateAppInstance' {..} =
    _salt `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateAppInstance where
  rnf CreateAppInstance' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateAppInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAppInstance where
  toJSON CreateAppInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Metadata" Data..=) Prelude.<$> metadata,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateAppInstance where
  toPath = Prelude.const "/app-instances"

instance Data.ToQuery CreateAppInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppInstanceResponse' smart constructor.
data CreateAppInstanceResponse = CreateAppInstanceResponse'
  { -- | The Amazon Resource Number (ARN) of the @AppInstance@.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'createAppInstanceResponse_appInstanceArn' - The Amazon Resource Number (ARN) of the @AppInstance@.
--
-- 'httpStatus', 'createAppInstanceResponse_httpStatus' - The response's http status code.
newCreateAppInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppInstanceResponse
newCreateAppInstanceResponse pHttpStatus_ =
  CreateAppInstanceResponse'
    { appInstanceArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the @AppInstance@.
createAppInstanceResponse_appInstanceArn :: Lens.Lens' CreateAppInstanceResponse (Prelude.Maybe Prelude.Text)
createAppInstanceResponse_appInstanceArn = Lens.lens (\CreateAppInstanceResponse' {appInstanceArn} -> appInstanceArn) (\s@CreateAppInstanceResponse' {} a -> s {appInstanceArn = a} :: CreateAppInstanceResponse)

-- | The response's http status code.
createAppInstanceResponse_httpStatus :: Lens.Lens' CreateAppInstanceResponse Prelude.Int
createAppInstanceResponse_httpStatus = Lens.lens (\CreateAppInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateAppInstanceResponse' {} a -> s {httpStatus = a} :: CreateAppInstanceResponse)

instance Prelude.NFData CreateAppInstanceResponse where
  rnf CreateAppInstanceResponse' {..} =
    Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf httpStatus
