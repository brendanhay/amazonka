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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteConfigurationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing configuration set.
--
-- A configuration set is a set of rules that you apply to voice and SMS
-- messages that you send. In a configuration set, you can specify a
-- destination for specific types of events related to voice and SMS
-- messages.
module Amazonka.PinpointSmsVoiceV2.DeleteConfigurationSet
  ( -- * Creating a Request
    DeleteConfigurationSet (..),
    newDeleteConfigurationSet,

    -- * Request Lenses
    deleteConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    DeleteConfigurationSetResponse (..),
    newDeleteConfigurationSetResponse,

    -- * Response Lenses
    deleteConfigurationSetResponse_configurationSetArn,
    deleteConfigurationSetResponse_configurationSetName,
    deleteConfigurationSetResponse_createdTimestamp,
    deleteConfigurationSetResponse_defaultMessageType,
    deleteConfigurationSetResponse_defaultSenderId,
    deleteConfigurationSetResponse_eventDestinations,
    deleteConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfigurationSet' smart constructor.
data DeleteConfigurationSet = DeleteConfigurationSet'
  { -- | The name of the configuration set or the configuration set ARN that you
    -- want to delete. The ConfigurationSetName and ConfigurationSetArn can be
    -- found using the DescribeConfigurationSets action.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteConfigurationSet_configurationSetName' - The name of the configuration set or the configuration set ARN that you
-- want to delete. The ConfigurationSetName and ConfigurationSetArn can be
-- found using the DescribeConfigurationSets action.
newDeleteConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteConfigurationSet
newDeleteConfigurationSet pConfigurationSetName_ =
  DeleteConfigurationSet'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set or the configuration set ARN that you
-- want to delete. The ConfigurationSetName and ConfigurationSetArn can be
-- found using the DescribeConfigurationSets action.
deleteConfigurationSet_configurationSetName :: Lens.Lens' DeleteConfigurationSet Prelude.Text
deleteConfigurationSet_configurationSetName = Lens.lens (\DeleteConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSet' {} a -> s {configurationSetName = a} :: DeleteConfigurationSet)

instance Core.AWSRequest DeleteConfigurationSet where
  type
    AWSResponse DeleteConfigurationSet =
      DeleteConfigurationSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConfigurationSetResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "DefaultMessageType")
            Prelude.<*> (x Data..?> "DefaultSenderId")
            Prelude.<*> ( x Data..?> "EventDestinations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConfigurationSet where
  hashWithSalt _salt DeleteConfigurationSet' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData DeleteConfigurationSet where
  rnf DeleteConfigurationSet' {..} =
    Prelude.rnf configurationSetName

instance Data.ToHeaders DeleteConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteConfigurationSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConfigurationSet where
  toJSON DeleteConfigurationSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              )
          ]
      )

instance Data.ToPath DeleteConfigurationSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationSetResponse' smart constructor.
data DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the deleted configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The time that the deleted configuration set was created in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The default message type of the configuration set that was deleted.
    defaultMessageType :: Prelude.Maybe MessageType,
    -- | The default Sender ID of the configuration set that was deleted.
    defaultSenderId :: Prelude.Maybe Prelude.Text,
    -- | An array of any EventDestination objects that were associated with the
    -- deleted configuration set.
    eventDestinations :: Prelude.Maybe [EventDestination],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetArn', 'deleteConfigurationSetResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the deleted configuration set.
--
-- 'configurationSetName', 'deleteConfigurationSetResponse_configurationSetName' - The name of the deleted configuration set.
--
-- 'createdTimestamp', 'deleteConfigurationSetResponse_createdTimestamp' - The time that the deleted configuration set was created in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'defaultMessageType', 'deleteConfigurationSetResponse_defaultMessageType' - The default message type of the configuration set that was deleted.
--
-- 'defaultSenderId', 'deleteConfigurationSetResponse_defaultSenderId' - The default Sender ID of the configuration set that was deleted.
--
-- 'eventDestinations', 'deleteConfigurationSetResponse_eventDestinations' - An array of any EventDestination objects that were associated with the
-- deleted configuration set.
--
-- 'httpStatus', 'deleteConfigurationSetResponse_httpStatus' - The response's http status code.
newDeleteConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfigurationSetResponse
newDeleteConfigurationSetResponse pHttpStatus_ =
  DeleteConfigurationSetResponse'
    { configurationSetArn =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      defaultMessageType = Prelude.Nothing,
      defaultSenderId = Prelude.Nothing,
      eventDestinations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted configuration set.
deleteConfigurationSetResponse_configurationSetArn :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_configurationSetArn = Lens.lens (\DeleteConfigurationSetResponse' {configurationSetArn} -> configurationSetArn) (\s@DeleteConfigurationSetResponse' {} a -> s {configurationSetArn = a} :: DeleteConfigurationSetResponse)

-- | The name of the deleted configuration set.
deleteConfigurationSetResponse_configurationSetName :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_configurationSetName = Lens.lens (\DeleteConfigurationSetResponse' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetResponse' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetResponse)

-- | The time that the deleted configuration set was created in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
deleteConfigurationSetResponse_createdTimestamp :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.UTCTime)
deleteConfigurationSetResponse_createdTimestamp = Lens.lens (\DeleteConfigurationSetResponse' {createdTimestamp} -> createdTimestamp) (\s@DeleteConfigurationSetResponse' {} a -> s {createdTimestamp = a} :: DeleteConfigurationSetResponse) Prelude.. Lens.mapping Data._Time

-- | The default message type of the configuration set that was deleted.
deleteConfigurationSetResponse_defaultMessageType :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe MessageType)
deleteConfigurationSetResponse_defaultMessageType = Lens.lens (\DeleteConfigurationSetResponse' {defaultMessageType} -> defaultMessageType) (\s@DeleteConfigurationSetResponse' {} a -> s {defaultMessageType = a} :: DeleteConfigurationSetResponse)

-- | The default Sender ID of the configuration set that was deleted.
deleteConfigurationSetResponse_defaultSenderId :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_defaultSenderId = Lens.lens (\DeleteConfigurationSetResponse' {defaultSenderId} -> defaultSenderId) (\s@DeleteConfigurationSetResponse' {} a -> s {defaultSenderId = a} :: DeleteConfigurationSetResponse)

-- | An array of any EventDestination objects that were associated with the
-- deleted configuration set.
deleteConfigurationSetResponse_eventDestinations :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe [EventDestination])
deleteConfigurationSetResponse_eventDestinations = Lens.lens (\DeleteConfigurationSetResponse' {eventDestinations} -> eventDestinations) (\s@DeleteConfigurationSetResponse' {} a -> s {eventDestinations = a} :: DeleteConfigurationSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteConfigurationSetResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetResponse Prelude.Int
deleteConfigurationSetResponse_httpStatus = Lens.lens (\DeleteConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetResponse)

instance
  Prelude.NFData
    DeleteConfigurationSetResponse
  where
  rnf DeleteConfigurationSetResponse' {..} =
    Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf defaultMessageType
      `Prelude.seq` Prelude.rnf defaultSenderId
      `Prelude.seq` Prelude.rnf eventDestinations
      `Prelude.seq` Prelude.rnf httpStatus
