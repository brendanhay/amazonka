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
-- Module      : Amazonka.PinpointSmsVoiceV2.SetDefaultMessageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the default message type on a configuration set.
--
-- Choose the category of SMS messages that you plan to send from this
-- account. If you send account-related messages or time-sensitive messages
-- such as one-time passcodes, choose __Transactional__. If you plan to
-- send messages that contain marketing material or other promotional
-- content, choose __Promotional__. This setting applies to your entire
-- Amazon Web Services account.
module Amazonka.PinpointSmsVoiceV2.SetDefaultMessageType
  ( -- * Creating a Request
    SetDefaultMessageType (..),
    newSetDefaultMessageType,

    -- * Request Lenses
    setDefaultMessageType_configurationSetName,
    setDefaultMessageType_messageType,

    -- * Destructuring the Response
    SetDefaultMessageTypeResponse (..),
    newSetDefaultMessageTypeResponse,

    -- * Response Lenses
    setDefaultMessageTypeResponse_configurationSetArn,
    setDefaultMessageTypeResponse_configurationSetName,
    setDefaultMessageTypeResponse_messageType,
    setDefaultMessageTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetDefaultMessageType' smart constructor.
data SetDefaultMessageType = SetDefaultMessageType'
  { -- | The configuration set to update with a new default message type. This
    -- field can be the ConsigurationSetName or ConfigurationSetArn.
    configurationSetName :: Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: MessageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultMessageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'setDefaultMessageType_configurationSetName' - The configuration set to update with a new default message type. This
-- field can be the ConsigurationSetName or ConfigurationSetArn.
--
-- 'messageType', 'setDefaultMessageType_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
newSetDefaultMessageType ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'messageType'
  MessageType ->
  SetDefaultMessageType
newSetDefaultMessageType
  pConfigurationSetName_
  pMessageType_ =
    SetDefaultMessageType'
      { configurationSetName =
          pConfigurationSetName_,
        messageType = pMessageType_
      }

-- | The configuration set to update with a new default message type. This
-- field can be the ConsigurationSetName or ConfigurationSetArn.
setDefaultMessageType_configurationSetName :: Lens.Lens' SetDefaultMessageType Prelude.Text
setDefaultMessageType_configurationSetName = Lens.lens (\SetDefaultMessageType' {configurationSetName} -> configurationSetName) (\s@SetDefaultMessageType' {} a -> s {configurationSetName = a} :: SetDefaultMessageType)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
setDefaultMessageType_messageType :: Lens.Lens' SetDefaultMessageType MessageType
setDefaultMessageType_messageType = Lens.lens (\SetDefaultMessageType' {messageType} -> messageType) (\s@SetDefaultMessageType' {} a -> s {messageType = a} :: SetDefaultMessageType)

instance Core.AWSRequest SetDefaultMessageType where
  type
    AWSResponse SetDefaultMessageType =
      SetDefaultMessageTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetDefaultMessageTypeResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "MessageType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetDefaultMessageType where
  hashWithSalt _salt SetDefaultMessageType' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` messageType

instance Prelude.NFData SetDefaultMessageType where
  rnf SetDefaultMessageType' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf messageType

instance Data.ToHeaders SetDefaultMessageType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SetDefaultMessageType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetDefaultMessageType where
  toJSON SetDefaultMessageType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              ),
            Prelude.Just ("MessageType" Data..= messageType)
          ]
      )

instance Data.ToPath SetDefaultMessageType where
  toPath = Prelude.const "/"

instance Data.ToQuery SetDefaultMessageType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDefaultMessageTypeResponse' smart constructor.
data SetDefaultMessageTypeResponse = SetDefaultMessageTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the updated configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set that was updated.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The new default message type of the configuration set.
    messageType :: Prelude.Maybe MessageType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultMessageTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'setDefaultMessageTypeResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the updated configuration set.
--
-- 'configurationSetName', 'setDefaultMessageTypeResponse_configurationSetName' - The name of the configuration set that was updated.
--
-- 'messageType', 'setDefaultMessageTypeResponse_messageType' - The new default message type of the configuration set.
--
-- 'httpStatus', 'setDefaultMessageTypeResponse_httpStatus' - The response's http status code.
newSetDefaultMessageTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetDefaultMessageTypeResponse
newSetDefaultMessageTypeResponse pHttpStatus_ =
  SetDefaultMessageTypeResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      messageType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated configuration set.
setDefaultMessageTypeResponse_configurationSetArn :: Lens.Lens' SetDefaultMessageTypeResponse (Prelude.Maybe Prelude.Text)
setDefaultMessageTypeResponse_configurationSetArn = Lens.lens (\SetDefaultMessageTypeResponse' {configurationSetArn} -> configurationSetArn) (\s@SetDefaultMessageTypeResponse' {} a -> s {configurationSetArn = a} :: SetDefaultMessageTypeResponse)

-- | The name of the configuration set that was updated.
setDefaultMessageTypeResponse_configurationSetName :: Lens.Lens' SetDefaultMessageTypeResponse (Prelude.Maybe Prelude.Text)
setDefaultMessageTypeResponse_configurationSetName = Lens.lens (\SetDefaultMessageTypeResponse' {configurationSetName} -> configurationSetName) (\s@SetDefaultMessageTypeResponse' {} a -> s {configurationSetName = a} :: SetDefaultMessageTypeResponse)

-- | The new default message type of the configuration set.
setDefaultMessageTypeResponse_messageType :: Lens.Lens' SetDefaultMessageTypeResponse (Prelude.Maybe MessageType)
setDefaultMessageTypeResponse_messageType = Lens.lens (\SetDefaultMessageTypeResponse' {messageType} -> messageType) (\s@SetDefaultMessageTypeResponse' {} a -> s {messageType = a} :: SetDefaultMessageTypeResponse)

-- | The response's http status code.
setDefaultMessageTypeResponse_httpStatus :: Lens.Lens' SetDefaultMessageTypeResponse Prelude.Int
setDefaultMessageTypeResponse_httpStatus = Lens.lens (\SetDefaultMessageTypeResponse' {httpStatus} -> httpStatus) (\s@SetDefaultMessageTypeResponse' {} a -> s {httpStatus = a} :: SetDefaultMessageTypeResponse)

instance Prelude.NFData SetDefaultMessageTypeResponse where
  rnf SetDefaultMessageTypeResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf httpStatus
