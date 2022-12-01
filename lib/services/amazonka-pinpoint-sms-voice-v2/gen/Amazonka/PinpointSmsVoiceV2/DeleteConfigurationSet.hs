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
    deleteConfigurationSetResponse_createdTimestamp,
    deleteConfigurationSetResponse_configurationSetName,
    deleteConfigurationSetResponse_defaultSenderId,
    deleteConfigurationSetResponse_configurationSetArn,
    deleteConfigurationSetResponse_defaultMessageType,
    deleteConfigurationSetResponse_eventDestinations,
    deleteConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "ConfigurationSetName")
            Prelude.<*> (x Core..?> "DefaultSenderId")
            Prelude.<*> (x Core..?> "ConfigurationSetArn")
            Prelude.<*> (x Core..?> "DefaultMessageType")
            Prelude.<*> ( x Core..?> "EventDestinations"
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

instance Core.ToHeaders DeleteConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PinpointSMSVoiceV2.DeleteConfigurationSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteConfigurationSet where
  toJSON DeleteConfigurationSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Core..= configurationSetName
              )
          ]
      )

instance Core.ToPath DeleteConfigurationSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationSetResponse' smart constructor.
data DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { -- | The time that the deleted configuration set was created in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the deleted configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The default Sender ID of the configuration set that was deleted.
    defaultSenderId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The default message type of the configuration set that was deleted.
    defaultMessageType :: Prelude.Maybe MessageType,
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
-- 'createdTimestamp', 'deleteConfigurationSetResponse_createdTimestamp' - The time that the deleted configuration set was created in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'configurationSetName', 'deleteConfigurationSetResponse_configurationSetName' - The name of the deleted configuration set.
--
-- 'defaultSenderId', 'deleteConfigurationSetResponse_defaultSenderId' - The default Sender ID of the configuration set that was deleted.
--
-- 'configurationSetArn', 'deleteConfigurationSetResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the deleted configuration set.
--
-- 'defaultMessageType', 'deleteConfigurationSetResponse_defaultMessageType' - The default message type of the configuration set that was deleted.
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
    { createdTimestamp =
        Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      defaultSenderId = Prelude.Nothing,
      configurationSetArn = Prelude.Nothing,
      defaultMessageType = Prelude.Nothing,
      eventDestinations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time that the deleted configuration set was created in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
deleteConfigurationSetResponse_createdTimestamp :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.UTCTime)
deleteConfigurationSetResponse_createdTimestamp = Lens.lens (\DeleteConfigurationSetResponse' {createdTimestamp} -> createdTimestamp) (\s@DeleteConfigurationSetResponse' {} a -> s {createdTimestamp = a} :: DeleteConfigurationSetResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the deleted configuration set.
deleteConfigurationSetResponse_configurationSetName :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_configurationSetName = Lens.lens (\DeleteConfigurationSetResponse' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetResponse' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetResponse)

-- | The default Sender ID of the configuration set that was deleted.
deleteConfigurationSetResponse_defaultSenderId :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_defaultSenderId = Lens.lens (\DeleteConfigurationSetResponse' {defaultSenderId} -> defaultSenderId) (\s@DeleteConfigurationSetResponse' {} a -> s {defaultSenderId = a} :: DeleteConfigurationSetResponse)

-- | The Amazon Resource Name (ARN) of the deleted configuration set.
deleteConfigurationSetResponse_configurationSetArn :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationSetResponse_configurationSetArn = Lens.lens (\DeleteConfigurationSetResponse' {configurationSetArn} -> configurationSetArn) (\s@DeleteConfigurationSetResponse' {} a -> s {configurationSetArn = a} :: DeleteConfigurationSetResponse)

-- | The default message type of the configuration set that was deleted.
deleteConfigurationSetResponse_defaultMessageType :: Lens.Lens' DeleteConfigurationSetResponse (Prelude.Maybe MessageType)
deleteConfigurationSetResponse_defaultMessageType = Lens.lens (\DeleteConfigurationSetResponse' {defaultMessageType} -> defaultMessageType) (\s@DeleteConfigurationSetResponse' {} a -> s {defaultMessageType = a} :: DeleteConfigurationSetResponse)

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
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf defaultSenderId
      `Prelude.seq` Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf defaultMessageType
      `Prelude.seq` Prelude.rnf eventDestinations
      `Prelude.seq` Prelude.rnf httpStatus
