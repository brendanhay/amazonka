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
-- Module      : Amazonka.PinpointSmsVoiceV2.CreateConfigurationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration set. After you create the configuration set,
-- you can add one or more event destinations to it.
--
-- A configuration set is a set of rules that you apply to the SMS and
-- voice messages that you send.
--
-- When you send a message, you can optionally specify a single
-- configuration set.
module Amazonka.PinpointSmsVoiceV2.CreateConfigurationSet
  ( -- * Creating a Request
    CreateConfigurationSet (..),
    newCreateConfigurationSet,

    -- * Request Lenses
    createConfigurationSet_clientToken,
    createConfigurationSet_tags,
    createConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    CreateConfigurationSetResponse (..),
    newCreateConfigurationSetResponse,

    -- * Response Lenses
    createConfigurationSetResponse_configurationSetArn,
    createConfigurationSetResponse_configurationSetName,
    createConfigurationSetResponse_createdTimestamp,
    createConfigurationSetResponse_tags,
    createConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfigurationSet' smart constructor.
data CreateConfigurationSet = CreateConfigurationSet'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don\'t specify a client token, a
    -- randomly generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array of key and value pair tags that\'s associated with the new
    -- configuration set.
    tags :: Prelude.Maybe [Tag],
    -- | The name to use for the new configuration set.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createConfigurationSet_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
--
-- 'tags', 'createConfigurationSet_tags' - An array of key and value pair tags that\'s associated with the new
-- configuration set.
--
-- 'configurationSetName', 'createConfigurationSet_configurationSetName' - The name to use for the new configuration set.
newCreateConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  CreateConfigurationSet
newCreateConfigurationSet pConfigurationSetName_ =
  CreateConfigurationSet'
    { clientToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
createConfigurationSet_clientToken :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe Prelude.Text)
createConfigurationSet_clientToken = Lens.lens (\CreateConfigurationSet' {clientToken} -> clientToken) (\s@CreateConfigurationSet' {} a -> s {clientToken = a} :: CreateConfigurationSet)

-- | An array of key and value pair tags that\'s associated with the new
-- configuration set.
createConfigurationSet_tags :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe [Tag])
createConfigurationSet_tags = Lens.lens (\CreateConfigurationSet' {tags} -> tags) (\s@CreateConfigurationSet' {} a -> s {tags = a} :: CreateConfigurationSet) Prelude.. Lens.mapping Lens.coerced

-- | The name to use for the new configuration set.
createConfigurationSet_configurationSetName :: Lens.Lens' CreateConfigurationSet Prelude.Text
createConfigurationSet_configurationSetName = Lens.lens (\CreateConfigurationSet' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSet' {} a -> s {configurationSetName = a} :: CreateConfigurationSet)

instance Core.AWSRequest CreateConfigurationSet where
  type
    AWSResponse CreateConfigurationSet =
      CreateConfigurationSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfigurationSetResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfigurationSet where
  hashWithSalt _salt CreateConfigurationSet' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData CreateConfigurationSet where
  rnf CreateConfigurationSet' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configurationSetName

instance Data.ToHeaders CreateConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.CreateConfigurationSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConfigurationSet where
  toJSON CreateConfigurationSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              )
          ]
      )

instance Data.ToPath CreateConfigurationSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfigurationSetResponse' smart constructor.
data CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the new configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration set was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | An array of key and value pair tags that\'s associated with the
    -- configuration set.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'createConfigurationSetResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the newly created configuration set.
--
-- 'configurationSetName', 'createConfigurationSetResponse_configurationSetName' - The name of the new configuration set.
--
-- 'createdTimestamp', 'createConfigurationSetResponse_createdTimestamp' - The time when the configuration set was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'tags', 'createConfigurationSetResponse_tags' - An array of key and value pair tags that\'s associated with the
-- configuration set.
--
-- 'httpStatus', 'createConfigurationSetResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationSetResponse
newCreateConfigurationSetResponse pHttpStatus_ =
  CreateConfigurationSetResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created configuration set.
createConfigurationSetResponse_configurationSetArn :: Lens.Lens' CreateConfigurationSetResponse (Prelude.Maybe Prelude.Text)
createConfigurationSetResponse_configurationSetArn = Lens.lens (\CreateConfigurationSetResponse' {configurationSetArn} -> configurationSetArn) (\s@CreateConfigurationSetResponse' {} a -> s {configurationSetArn = a} :: CreateConfigurationSetResponse)

-- | The name of the new configuration set.
createConfigurationSetResponse_configurationSetName :: Lens.Lens' CreateConfigurationSetResponse (Prelude.Maybe Prelude.Text)
createConfigurationSetResponse_configurationSetName = Lens.lens (\CreateConfigurationSetResponse' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSetResponse' {} a -> s {configurationSetName = a} :: CreateConfigurationSetResponse)

-- | The time when the configuration set was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
createConfigurationSetResponse_createdTimestamp :: Lens.Lens' CreateConfigurationSetResponse (Prelude.Maybe Prelude.UTCTime)
createConfigurationSetResponse_createdTimestamp = Lens.lens (\CreateConfigurationSetResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateConfigurationSetResponse' {} a -> s {createdTimestamp = a} :: CreateConfigurationSetResponse) Prelude.. Lens.mapping Data._Time

-- | An array of key and value pair tags that\'s associated with the
-- configuration set.
createConfigurationSetResponse_tags :: Lens.Lens' CreateConfigurationSetResponse (Prelude.Maybe [Tag])
createConfigurationSetResponse_tags = Lens.lens (\CreateConfigurationSetResponse' {tags} -> tags) (\s@CreateConfigurationSetResponse' {} a -> s {tags = a} :: CreateConfigurationSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createConfigurationSetResponse_httpStatus :: Lens.Lens' CreateConfigurationSetResponse Prelude.Int
createConfigurationSetResponse_httpStatus = Lens.lens (\CreateConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetResponse)

instance
  Prelude.NFData
    CreateConfigurationSetResponse
  where
  rnf CreateConfigurationSetResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
