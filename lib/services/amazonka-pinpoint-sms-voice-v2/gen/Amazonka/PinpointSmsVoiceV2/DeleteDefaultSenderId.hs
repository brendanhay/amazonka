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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteDefaultSenderId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing default sender ID on a configuration set.
--
-- A default sender ID is the identity that appears on recipients\' devices
-- when they receive SMS messages. Support for sender ID capabilities
-- varies by country or region.
module Amazonka.PinpointSmsVoiceV2.DeleteDefaultSenderId
  ( -- * Creating a Request
    DeleteDefaultSenderId (..),
    newDeleteDefaultSenderId,

    -- * Request Lenses
    deleteDefaultSenderId_configurationSetName,

    -- * Destructuring the Response
    DeleteDefaultSenderIdResponse (..),
    newDeleteDefaultSenderIdResponse,

    -- * Response Lenses
    deleteDefaultSenderIdResponse_configurationSetArn,
    deleteDefaultSenderIdResponse_configurationSetName,
    deleteDefaultSenderIdResponse_senderId,
    deleteDefaultSenderIdResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDefaultSenderId' smart constructor.
data DeleteDefaultSenderId = DeleteDefaultSenderId'
  { -- | The name of the configuration set or the configuration set Amazon
    -- Resource Name (ARN) to delete the default sender ID from. The
    -- ConfigurationSetName and ConfigurationSetArn can be found using the
    -- DescribeConfigurationSets action.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDefaultSenderId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteDefaultSenderId_configurationSetName' - The name of the configuration set or the configuration set Amazon
-- Resource Name (ARN) to delete the default sender ID from. The
-- ConfigurationSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
newDeleteDefaultSenderId ::
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteDefaultSenderId
newDeleteDefaultSenderId pConfigurationSetName_ =
  DeleteDefaultSenderId'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set or the configuration set Amazon
-- Resource Name (ARN) to delete the default sender ID from. The
-- ConfigurationSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
deleteDefaultSenderId_configurationSetName :: Lens.Lens' DeleteDefaultSenderId Prelude.Text
deleteDefaultSenderId_configurationSetName = Lens.lens (\DeleteDefaultSenderId' {configurationSetName} -> configurationSetName) (\s@DeleteDefaultSenderId' {} a -> s {configurationSetName = a} :: DeleteDefaultSenderId)

instance Core.AWSRequest DeleteDefaultSenderId where
  type
    AWSResponse DeleteDefaultSenderId =
      DeleteDefaultSenderIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDefaultSenderIdResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "SenderId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDefaultSenderId where
  hashWithSalt _salt DeleteDefaultSenderId' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData DeleteDefaultSenderId where
  rnf DeleteDefaultSenderId' {..} =
    Prelude.rnf configurationSetName

instance Data.ToHeaders DeleteDefaultSenderId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteDefaultSenderId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDefaultSenderId where
  toJSON DeleteDefaultSenderId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              )
          ]
      )

instance Data.ToPath DeleteDefaultSenderId where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDefaultSenderId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDefaultSenderIdResponse' smart constructor.
data DeleteDefaultSenderIdResponse = DeleteDefaultSenderIdResponse'
  { -- | The Amazon Resource Name (ARN) of the configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The current sender ID for the configuration set.
    senderId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDefaultSenderIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'deleteDefaultSenderIdResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the configuration set.
--
-- 'configurationSetName', 'deleteDefaultSenderIdResponse_configurationSetName' - The name of the configuration set.
--
-- 'senderId', 'deleteDefaultSenderIdResponse_senderId' - The current sender ID for the configuration set.
--
-- 'httpStatus', 'deleteDefaultSenderIdResponse_httpStatus' - The response's http status code.
newDeleteDefaultSenderIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDefaultSenderIdResponse
newDeleteDefaultSenderIdResponse pHttpStatus_ =
  DeleteDefaultSenderIdResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      senderId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configuration set.
deleteDefaultSenderIdResponse_configurationSetArn :: Lens.Lens' DeleteDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
deleteDefaultSenderIdResponse_configurationSetArn = Lens.lens (\DeleteDefaultSenderIdResponse' {configurationSetArn} -> configurationSetArn) (\s@DeleteDefaultSenderIdResponse' {} a -> s {configurationSetArn = a} :: DeleteDefaultSenderIdResponse)

-- | The name of the configuration set.
deleteDefaultSenderIdResponse_configurationSetName :: Lens.Lens' DeleteDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
deleteDefaultSenderIdResponse_configurationSetName = Lens.lens (\DeleteDefaultSenderIdResponse' {configurationSetName} -> configurationSetName) (\s@DeleteDefaultSenderIdResponse' {} a -> s {configurationSetName = a} :: DeleteDefaultSenderIdResponse)

-- | The current sender ID for the configuration set.
deleteDefaultSenderIdResponse_senderId :: Lens.Lens' DeleteDefaultSenderIdResponse (Prelude.Maybe Prelude.Text)
deleteDefaultSenderIdResponse_senderId = Lens.lens (\DeleteDefaultSenderIdResponse' {senderId} -> senderId) (\s@DeleteDefaultSenderIdResponse' {} a -> s {senderId = a} :: DeleteDefaultSenderIdResponse)

-- | The response's http status code.
deleteDefaultSenderIdResponse_httpStatus :: Lens.Lens' DeleteDefaultSenderIdResponse Prelude.Int
deleteDefaultSenderIdResponse_httpStatus = Lens.lens (\DeleteDefaultSenderIdResponse' {httpStatus} -> httpStatus) (\s@DeleteDefaultSenderIdResponse' {} a -> s {httpStatus = a} :: DeleteDefaultSenderIdResponse)

instance Prelude.NFData DeleteDefaultSenderIdResponse where
  rnf DeleteDefaultSenderIdResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf senderId
      `Prelude.seq` Prelude.rnf httpStatus
