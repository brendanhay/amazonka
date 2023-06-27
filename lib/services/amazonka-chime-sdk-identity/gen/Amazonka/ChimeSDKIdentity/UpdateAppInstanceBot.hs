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
-- Module      : Amazonka.ChimeSDKIdentity.UpdateAppInstanceBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and metadata of an @AppInstanceBot@.
module Amazonka.ChimeSDKIdentity.UpdateAppInstanceBot
  ( -- * Creating a Request
    UpdateAppInstanceBot (..),
    newUpdateAppInstanceBot,

    -- * Request Lenses
    updateAppInstanceBot_configuration,
    updateAppInstanceBot_appInstanceBotArn,
    updateAppInstanceBot_name,
    updateAppInstanceBot_metadata,

    -- * Destructuring the Response
    UpdateAppInstanceBotResponse (..),
    newUpdateAppInstanceBotResponse,

    -- * Response Lenses
    updateAppInstanceBotResponse_appInstanceBotArn,
    updateAppInstanceBotResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppInstanceBot' smart constructor.
data UpdateAppInstanceBot = UpdateAppInstanceBot'
  { -- | The configuration for the bot update.
    configuration :: Prelude.Maybe Configuration,
    -- | The ARN of the @AppInstanceBot@.
    appInstanceBotArn :: Prelude.Text,
    -- | The name of the @AppInstanceBot@.
    name :: Data.Sensitive Prelude.Text,
    -- | The metadata of the @AppInstanceBot@.
    metadata :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateAppInstanceBot_configuration' - The configuration for the bot update.
--
-- 'appInstanceBotArn', 'updateAppInstanceBot_appInstanceBotArn' - The ARN of the @AppInstanceBot@.
--
-- 'name', 'updateAppInstanceBot_name' - The name of the @AppInstanceBot@.
--
-- 'metadata', 'updateAppInstanceBot_metadata' - The metadata of the @AppInstanceBot@.
newUpdateAppInstanceBot ::
  -- | 'appInstanceBotArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'metadata'
  Prelude.Text ->
  UpdateAppInstanceBot
newUpdateAppInstanceBot
  pAppInstanceBotArn_
  pName_
  pMetadata_ =
    UpdateAppInstanceBot'
      { configuration =
          Prelude.Nothing,
        appInstanceBotArn = pAppInstanceBotArn_,
        name = Data._Sensitive Lens.# pName_,
        metadata = Data._Sensitive Lens.# pMetadata_
      }

-- | The configuration for the bot update.
updateAppInstanceBot_configuration :: Lens.Lens' UpdateAppInstanceBot (Prelude.Maybe Configuration)
updateAppInstanceBot_configuration = Lens.lens (\UpdateAppInstanceBot' {configuration} -> configuration) (\s@UpdateAppInstanceBot' {} a -> s {configuration = a} :: UpdateAppInstanceBot)

-- | The ARN of the @AppInstanceBot@.
updateAppInstanceBot_appInstanceBotArn :: Lens.Lens' UpdateAppInstanceBot Prelude.Text
updateAppInstanceBot_appInstanceBotArn = Lens.lens (\UpdateAppInstanceBot' {appInstanceBotArn} -> appInstanceBotArn) (\s@UpdateAppInstanceBot' {} a -> s {appInstanceBotArn = a} :: UpdateAppInstanceBot)

-- | The name of the @AppInstanceBot@.
updateAppInstanceBot_name :: Lens.Lens' UpdateAppInstanceBot Prelude.Text
updateAppInstanceBot_name = Lens.lens (\UpdateAppInstanceBot' {name} -> name) (\s@UpdateAppInstanceBot' {} a -> s {name = a} :: UpdateAppInstanceBot) Prelude.. Data._Sensitive

-- | The metadata of the @AppInstanceBot@.
updateAppInstanceBot_metadata :: Lens.Lens' UpdateAppInstanceBot Prelude.Text
updateAppInstanceBot_metadata = Lens.lens (\UpdateAppInstanceBot' {metadata} -> metadata) (\s@UpdateAppInstanceBot' {} a -> s {metadata = a} :: UpdateAppInstanceBot) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdateAppInstanceBot where
  type
    AWSResponse UpdateAppInstanceBot =
      UpdateAppInstanceBotResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppInstanceBotResponse'
            Prelude.<$> (x Data..?> "AppInstanceBotArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAppInstanceBot where
  hashWithSalt _salt UpdateAppInstanceBot' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` appInstanceBotArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData UpdateAppInstanceBot where
  rnf UpdateAppInstanceBot' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf appInstanceBotArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metadata

instance Data.ToHeaders UpdateAppInstanceBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAppInstanceBot where
  toJSON UpdateAppInstanceBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configuration" Data..=) Prelude.<$> configuration,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Metadata" Data..= metadata)
          ]
      )

instance Data.ToPath UpdateAppInstanceBot where
  toPath UpdateAppInstanceBot' {..} =
    Prelude.mconcat
      ["/app-instance-bots/", Data.toBS appInstanceBotArn]

instance Data.ToQuery UpdateAppInstanceBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppInstanceBotResponse' smart constructor.
data UpdateAppInstanceBotResponse = UpdateAppInstanceBotResponse'
  { -- | The ARN of the @AppInstanceBot@.
    appInstanceBotArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'updateAppInstanceBotResponse_appInstanceBotArn' - The ARN of the @AppInstanceBot@.
--
-- 'httpStatus', 'updateAppInstanceBotResponse_httpStatus' - The response's http status code.
newUpdateAppInstanceBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAppInstanceBotResponse
newUpdateAppInstanceBotResponse pHttpStatus_ =
  UpdateAppInstanceBotResponse'
    { appInstanceBotArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the @AppInstanceBot@.
updateAppInstanceBotResponse_appInstanceBotArn :: Lens.Lens' UpdateAppInstanceBotResponse (Prelude.Maybe Prelude.Text)
updateAppInstanceBotResponse_appInstanceBotArn = Lens.lens (\UpdateAppInstanceBotResponse' {appInstanceBotArn} -> appInstanceBotArn) (\s@UpdateAppInstanceBotResponse' {} a -> s {appInstanceBotArn = a} :: UpdateAppInstanceBotResponse)

-- | The response's http status code.
updateAppInstanceBotResponse_httpStatus :: Lens.Lens' UpdateAppInstanceBotResponse Prelude.Int
updateAppInstanceBotResponse_httpStatus = Lens.lens (\UpdateAppInstanceBotResponse' {httpStatus} -> httpStatus) (\s@UpdateAppInstanceBotResponse' {} a -> s {httpStatus = a} :: UpdateAppInstanceBotResponse)

instance Prelude.NFData UpdateAppInstanceBotResponse where
  rnf UpdateAppInstanceBotResponse' {..} =
    Prelude.rnf appInstanceBotArn
      `Prelude.seq` Prelude.rnf httpStatus
