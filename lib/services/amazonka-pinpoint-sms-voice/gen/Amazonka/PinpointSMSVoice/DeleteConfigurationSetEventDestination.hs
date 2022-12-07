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
-- Module      : Amazonka.PinpointSMSVoice.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an event destination in a configuration set.
module Amazonka.PinpointSMSVoice.DeleteConfigurationSetEventDestination
  ( -- * Creating a Request
    DeleteConfigurationSetEventDestination (..),
    newDeleteConfigurationSetEventDestination,

    -- * Request Lenses
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestination_configurationSetName,

    -- * Destructuring the Response
    DeleteConfigurationSetEventDestinationResponse (..),
    newDeleteConfigurationSetEventDestinationResponse,

    -- * Response Lenses
    deleteConfigurationSetEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSMSVoice.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { -- | EventDestinationName
    eventDestinationName :: Prelude.Text,
    -- | ConfigurationSetName
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDestinationName', 'deleteConfigurationSetEventDestination_eventDestinationName' - EventDestinationName
--
-- 'configurationSetName', 'deleteConfigurationSetEventDestination_configurationSetName' - ConfigurationSetName
newDeleteConfigurationSetEventDestination ::
  -- | 'eventDestinationName'
  Prelude.Text ->
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteConfigurationSetEventDestination
newDeleteConfigurationSetEventDestination
  pEventDestinationName_
  pConfigurationSetName_ =
    DeleteConfigurationSetEventDestination'
      { eventDestinationName =
          pEventDestinationName_,
        configurationSetName =
          pConfigurationSetName_
      }

-- | EventDestinationName
deleteConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' DeleteConfigurationSetEventDestination Prelude.Text
deleteConfigurationSetEventDestination_eventDestinationName = Lens.lens (\DeleteConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@DeleteConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: DeleteConfigurationSetEventDestination)

-- | ConfigurationSetName
deleteConfigurationSetEventDestination_configurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Prelude.Text
deleteConfigurationSetEventDestination_configurationSetName = Lens.lens (\DeleteConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetEventDestination)

instance
  Core.AWSRequest
    DeleteConfigurationSetEventDestination
  where
  type
    AWSResponse
      DeleteConfigurationSetEventDestination =
      DeleteConfigurationSetEventDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteConfigurationSetEventDestination
  where
  hashWithSalt
    _salt
    DeleteConfigurationSetEventDestination' {..} =
      _salt `Prelude.hashWithSalt` eventDestinationName
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestination
  where
  rnf DeleteConfigurationSetEventDestination' {..} =
    Prelude.rnf eventDestinationName
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    DeleteConfigurationSetEventDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteConfigurationSetEventDestination
  where
  toPath DeleteConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v1/sms-voice/configuration-sets/",
        Data.toBS configurationSetName,
        "/event-destinations/",
        Data.toBS eventDestinationName
      ]

instance
  Data.ToQuery
    DeleteConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An empty object that indicates that the event destination was deleted
-- successfully.
--
-- /See:/ 'newDeleteConfigurationSetEventDestinationResponse' smart constructor.
data DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfigurationSetEventDestinationResponse_httpStatus' - The response's http status code.
newDeleteConfigurationSetEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfigurationSetEventDestinationResponse
newDeleteConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    DeleteConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetEventDestinationResponse Prelude.Int
deleteConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\DeleteConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetEventDestinationResponse)

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestinationResponse
  where
  rnf
    DeleteConfigurationSetEventDestinationResponse' {..} =
      Prelude.rnf httpStatus
