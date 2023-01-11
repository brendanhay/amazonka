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
-- Module      : Amazonka.PinpointSmsVoiceV2.SetDefaultSenderId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets default sender ID on a configuration set.
--
-- When sending a text message to a destination country that supports
-- sender IDs, the default sender ID on the configuration set specified
-- will be used if no dedicated origination phone numbers or registered
-- sender IDs are available in your account.
module Amazonka.PinpointSmsVoiceV2.SetDefaultSenderId
  ( -- * Creating a Request
    SetDefaultSenderId (..),
    newSetDefaultSenderId,

    -- * Request Lenses
    setDefaultSenderId_configurationSetName,
    setDefaultSenderId_senderId,

    -- * Destructuring the Response
    SetDefaultSenderIdResponse (..),
    newSetDefaultSenderIdResponse,

    -- * Response Lenses
    setDefaultSenderIdResponse_configurationSetArn,
    setDefaultSenderIdResponse_configurationSetName,
    setDefaultSenderIdResponse_senderId,
    setDefaultSenderIdResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetDefaultSenderId' smart constructor.
data SetDefaultSenderId = SetDefaultSenderId'
  { -- | The configuration set to updated with a new default SenderId. This field
    -- can be the ConsigurationSetName or ConfigurationSetArn.
    configurationSetName :: Prelude.Text,
    -- | The current sender ID for the configuration set. When sending a text
    -- message to a destination country which supports SenderIds, the default
    -- sender ID on the configuration set specified on SendTextMessage will be
    -- used if no dedicated origination phone numbers or registered SenderIds
    -- are available in your account, instead of a generic sender ID, such as
    -- \'NOTICE\'.
    senderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultSenderId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'setDefaultSenderId_configurationSetName' - The configuration set to updated with a new default SenderId. This field
-- can be the ConsigurationSetName or ConfigurationSetArn.
--
-- 'senderId', 'setDefaultSenderId_senderId' - The current sender ID for the configuration set. When sending a text
-- message to a destination country which supports SenderIds, the default
-- sender ID on the configuration set specified on SendTextMessage will be
-- used if no dedicated origination phone numbers or registered SenderIds
-- are available in your account, instead of a generic sender ID, such as
-- \'NOTICE\'.
newSetDefaultSenderId ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'senderId'
  Prelude.Text ->
  SetDefaultSenderId
newSetDefaultSenderId
  pConfigurationSetName_
  pSenderId_ =
    SetDefaultSenderId'
      { configurationSetName =
          pConfigurationSetName_,
        senderId = pSenderId_
      }

-- | The configuration set to updated with a new default SenderId. This field
-- can be the ConsigurationSetName or ConfigurationSetArn.
setDefaultSenderId_configurationSetName :: Lens.Lens' SetDefaultSenderId Prelude.Text
setDefaultSenderId_configurationSetName = Lens.lens (\SetDefaultSenderId' {configurationSetName} -> configurationSetName) (\s@SetDefaultSenderId' {} a -> s {configurationSetName = a} :: SetDefaultSenderId)

-- | The current sender ID for the configuration set. When sending a text
-- message to a destination country which supports SenderIds, the default
-- sender ID on the configuration set specified on SendTextMessage will be
-- used if no dedicated origination phone numbers or registered SenderIds
-- are available in your account, instead of a generic sender ID, such as
-- \'NOTICE\'.
setDefaultSenderId_senderId :: Lens.Lens' SetDefaultSenderId Prelude.Text
setDefaultSenderId_senderId = Lens.lens (\SetDefaultSenderId' {senderId} -> senderId) (\s@SetDefaultSenderId' {} a -> s {senderId = a} :: SetDefaultSenderId)

instance Core.AWSRequest SetDefaultSenderId where
  type
    AWSResponse SetDefaultSenderId =
      SetDefaultSenderIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetDefaultSenderIdResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "SenderId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetDefaultSenderId where
  hashWithSalt _salt SetDefaultSenderId' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` senderId

instance Prelude.NFData SetDefaultSenderId where
  rnf SetDefaultSenderId' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf senderId

instance Data.ToHeaders SetDefaultSenderId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SetDefaultSenderId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetDefaultSenderId where
  toJSON SetDefaultSenderId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              ),
            Prelude.Just ("SenderId" Data..= senderId)
          ]
      )

instance Data.ToPath SetDefaultSenderId where
  toPath = Prelude.const "/"

instance Data.ToQuery SetDefaultSenderId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDefaultSenderIdResponse' smart constructor.
data SetDefaultSenderIdResponse = SetDefaultSenderIdResponse'
  { -- | The Amazon Resource Name (ARN) of the updated configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set that was updated.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The default sender ID to set for the ConfigurationSet.
    senderId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultSenderIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'setDefaultSenderIdResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the updated configuration set.
--
-- 'configurationSetName', 'setDefaultSenderIdResponse_configurationSetName' - The name of the configuration set that was updated.
--
-- 'senderId', 'setDefaultSenderIdResponse_senderId' - The default sender ID to set for the ConfigurationSet.
--
-- 'httpStatus', 'setDefaultSenderIdResponse_httpStatus' - The response's http status code.
newSetDefaultSenderIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetDefaultSenderIdResponse
newSetDefaultSenderIdResponse pHttpStatus_ =
  SetDefaultSenderIdResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      senderId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated configuration set.
setDefaultSenderIdResponse_configurationSetArn :: Lens.Lens' SetDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
setDefaultSenderIdResponse_configurationSetArn = Lens.lens (\SetDefaultSenderIdResponse' {configurationSetArn} -> configurationSetArn) (\s@SetDefaultSenderIdResponse' {} a -> s {configurationSetArn = a} :: SetDefaultSenderIdResponse)

-- | The name of the configuration set that was updated.
setDefaultSenderIdResponse_configurationSetName :: Lens.Lens' SetDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
setDefaultSenderIdResponse_configurationSetName = Lens.lens (\SetDefaultSenderIdResponse' {configurationSetName} -> configurationSetName) (\s@SetDefaultSenderIdResponse' {} a -> s {configurationSetName = a} :: SetDefaultSenderIdResponse)

-- | The default sender ID to set for the ConfigurationSet.
setDefaultSenderIdResponse_senderId :: Lens.Lens' SetDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
setDefaultSenderIdResponse_senderId = Lens.lens (\SetDefaultSenderIdResponse' {senderId} -> senderId) (\s@SetDefaultSenderIdResponse' {} a -> s {senderId = a} :: SetDefaultSenderIdResponse)

-- | The response's http status code.
setDefaultSenderIdResponse_httpStatus :: Lens.Lens' SetDefaultSenderIdResponse Prelude.Int
setDefaultSenderIdResponse_httpStatus = Lens.lens (\SetDefaultSenderIdResponse' {httpStatus} -> httpStatus) (\s@SetDefaultSenderIdResponse' {} a -> s {httpStatus = a} :: SetDefaultSenderIdResponse)

instance Prelude.NFData SetDefaultSenderIdResponse where
  rnf SetDefaultSenderIdResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf senderId
      `Prelude.seq` Prelude.rnf httpStatus
