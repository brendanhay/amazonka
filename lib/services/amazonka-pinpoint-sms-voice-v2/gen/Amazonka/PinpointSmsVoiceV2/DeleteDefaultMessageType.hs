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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteDefaultMessageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing default message type on a configuration set.
--
-- A message type is a type of messages that you plan to send. If you send
-- account-related messages or time-sensitive messages such as one-time
-- passcodes, choose __Transactional__. If you plan to send messages that
-- contain marketing material or other promotional content, choose
-- __Promotional__. This setting applies to your entire Amazon Web Services
-- account.
module Amazonka.PinpointSmsVoiceV2.DeleteDefaultMessageType
  ( -- * Creating a Request
    DeleteDefaultMessageType (..),
    newDeleteDefaultMessageType,

    -- * Request Lenses
    deleteDefaultMessageType_configurationSetName,

    -- * Destructuring the Response
    DeleteDefaultMessageTypeResponse (..),
    newDeleteDefaultMessageTypeResponse,

    -- * Response Lenses
    deleteDefaultMessageTypeResponse_messageType,
    deleteDefaultMessageTypeResponse_configurationSetName,
    deleteDefaultMessageTypeResponse_configurationSetArn,
    deleteDefaultMessageTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDefaultMessageType' smart constructor.
data DeleteDefaultMessageType = DeleteDefaultMessageType'
  { -- | The name of the configuration set or the configuration set Amazon
    -- Resource Name (ARN) to delete the default message type from. The
    -- ConfigurationSetName and ConfigurationSetArn can be found using the
    -- DescribeConfigurationSets action.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDefaultMessageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteDefaultMessageType_configurationSetName' - The name of the configuration set or the configuration set Amazon
-- Resource Name (ARN) to delete the default message type from. The
-- ConfigurationSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
newDeleteDefaultMessageType ::
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteDefaultMessageType
newDeleteDefaultMessageType pConfigurationSetName_ =
  DeleteDefaultMessageType'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set or the configuration set Amazon
-- Resource Name (ARN) to delete the default message type from. The
-- ConfigurationSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
deleteDefaultMessageType_configurationSetName :: Lens.Lens' DeleteDefaultMessageType Prelude.Text
deleteDefaultMessageType_configurationSetName = Lens.lens (\DeleteDefaultMessageType' {configurationSetName} -> configurationSetName) (\s@DeleteDefaultMessageType' {} a -> s {configurationSetName = a} :: DeleteDefaultMessageType)

instance Core.AWSRequest DeleteDefaultMessageType where
  type
    AWSResponse DeleteDefaultMessageType =
      DeleteDefaultMessageTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDefaultMessageTypeResponse'
            Prelude.<$> (x Data..?> "MessageType")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDefaultMessageType where
  hashWithSalt _salt DeleteDefaultMessageType' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData DeleteDefaultMessageType where
  rnf DeleteDefaultMessageType' {..} =
    Prelude.rnf configurationSetName

instance Data.ToHeaders DeleteDefaultMessageType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteDefaultMessageType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDefaultMessageType where
  toJSON DeleteDefaultMessageType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              )
          ]
      )

instance Data.ToPath DeleteDefaultMessageType where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDefaultMessageType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDefaultMessageTypeResponse' smart constructor.
data DeleteDefaultMessageTypeResponse = DeleteDefaultMessageTypeResponse'
  { -- | The current message type for the configuration set.
    messageType :: Prelude.Maybe MessageType,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDefaultMessageTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageType', 'deleteDefaultMessageTypeResponse_messageType' - The current message type for the configuration set.
--
-- 'configurationSetName', 'deleteDefaultMessageTypeResponse_configurationSetName' - The name of the configuration set.
--
-- 'configurationSetArn', 'deleteDefaultMessageTypeResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the configuration set.
--
-- 'httpStatus', 'deleteDefaultMessageTypeResponse_httpStatus' - The response's http status code.
newDeleteDefaultMessageTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDefaultMessageTypeResponse
newDeleteDefaultMessageTypeResponse pHttpStatus_ =
  DeleteDefaultMessageTypeResponse'
    { messageType =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      configurationSetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current message type for the configuration set.
deleteDefaultMessageTypeResponse_messageType :: Lens.Lens' DeleteDefaultMessageTypeResponse (Prelude.Maybe MessageType)
deleteDefaultMessageTypeResponse_messageType = Lens.lens (\DeleteDefaultMessageTypeResponse' {messageType} -> messageType) (\s@DeleteDefaultMessageTypeResponse' {} a -> s {messageType = a} :: DeleteDefaultMessageTypeResponse)

-- | The name of the configuration set.
deleteDefaultMessageTypeResponse_configurationSetName :: Lens.Lens' DeleteDefaultMessageTypeResponse (Prelude.Maybe Prelude.Text)
deleteDefaultMessageTypeResponse_configurationSetName = Lens.lens (\DeleteDefaultMessageTypeResponse' {configurationSetName} -> configurationSetName) (\s@DeleteDefaultMessageTypeResponse' {} a -> s {configurationSetName = a} :: DeleteDefaultMessageTypeResponse)

-- | The Amazon Resource Name (ARN) of the configuration set.
deleteDefaultMessageTypeResponse_configurationSetArn :: Lens.Lens' DeleteDefaultMessageTypeResponse (Prelude.Maybe Prelude.Text)
deleteDefaultMessageTypeResponse_configurationSetArn = Lens.lens (\DeleteDefaultMessageTypeResponse' {configurationSetArn} -> configurationSetArn) (\s@DeleteDefaultMessageTypeResponse' {} a -> s {configurationSetArn = a} :: DeleteDefaultMessageTypeResponse)

-- | The response's http status code.
deleteDefaultMessageTypeResponse_httpStatus :: Lens.Lens' DeleteDefaultMessageTypeResponse Prelude.Int
deleteDefaultMessageTypeResponse_httpStatus = Lens.lens (\DeleteDefaultMessageTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteDefaultMessageTypeResponse' {} a -> s {httpStatus = a} :: DeleteDefaultMessageTypeResponse)

instance
  Prelude.NFData
    DeleteDefaultMessageTypeResponse
  where
  rnf DeleteDefaultMessageTypeResponse' {..} =
    Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf httpStatus
