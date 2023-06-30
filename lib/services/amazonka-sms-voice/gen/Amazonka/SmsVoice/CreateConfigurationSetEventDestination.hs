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
-- Module      : Amazonka.SmsVoice.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new event destination in a configuration set.
module Amazonka.SmsVoice.CreateConfigurationSetEventDestination
  ( -- * Creating a Request
    CreateConfigurationSetEventDestination (..),
    newCreateConfigurationSetEventDestination,

    -- * Request Lenses
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_configurationSetName,

    -- * Destructuring the Response
    CreateConfigurationSetEventDestinationResponse (..),
    newCreateConfigurationSetEventDestinationResponse,

    -- * Response Lenses
    createConfigurationSetEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SmsVoice.Types

-- | Create a new event destination in a configuration set.
--
-- /See:/ 'newCreateConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { eventDestination :: Prelude.Maybe EventDestinationDefinition,
    -- | A name that identifies the event destination.
    eventDestinationName :: Prelude.Maybe Prelude.Text,
    -- | ConfigurationSetName
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDestination', 'createConfigurationSetEventDestination_eventDestination' - Undocumented member.
--
-- 'eventDestinationName', 'createConfigurationSetEventDestination_eventDestinationName' - A name that identifies the event destination.
--
-- 'configurationSetName', 'createConfigurationSetEventDestination_configurationSetName' - ConfigurationSetName
newCreateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  CreateConfigurationSetEventDestination
newCreateConfigurationSetEventDestination
  pConfigurationSetName_ =
    CreateConfigurationSetEventDestination'
      { eventDestination =
          Prelude.Nothing,
        eventDestinationName =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | Undocumented member.
createConfigurationSetEventDestination_eventDestination :: Lens.Lens' CreateConfigurationSetEventDestination (Prelude.Maybe EventDestinationDefinition)
createConfigurationSetEventDestination_eventDestination = Lens.lens (\CreateConfigurationSetEventDestination' {eventDestination} -> eventDestination) (\s@CreateConfigurationSetEventDestination' {} a -> s {eventDestination = a} :: CreateConfigurationSetEventDestination)

-- | A name that identifies the event destination.
createConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' CreateConfigurationSetEventDestination (Prelude.Maybe Prelude.Text)
createConfigurationSetEventDestination_eventDestinationName = Lens.lens (\CreateConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@CreateConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: CreateConfigurationSetEventDestination)

-- | ConfigurationSetName
createConfigurationSetEventDestination_configurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Prelude.Text
createConfigurationSetEventDestination_configurationSetName = Lens.lens (\CreateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: CreateConfigurationSetEventDestination)

instance
  Core.AWSRequest
    CreateConfigurationSetEventDestination
  where
  type
    AWSResponse
      CreateConfigurationSetEventDestination =
      CreateConfigurationSetEventDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConfigurationSetEventDestination
  where
  hashWithSalt
    _salt
    CreateConfigurationSetEventDestination' {..} =
      _salt
        `Prelude.hashWithSalt` eventDestination
        `Prelude.hashWithSalt` eventDestinationName
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    CreateConfigurationSetEventDestination
  where
  rnf CreateConfigurationSetEventDestination' {..} =
    Prelude.rnf eventDestination
      `Prelude.seq` Prelude.rnf eventDestinationName
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    CreateConfigurationSetEventDestination
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
  Data.ToJSON
    CreateConfigurationSetEventDestination
  where
  toJSON CreateConfigurationSetEventDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventDestination" Data..=)
              Prelude.<$> eventDestination,
            ("EventDestinationName" Data..=)
              Prelude.<$> eventDestinationName
          ]
      )

instance
  Data.ToPath
    CreateConfigurationSetEventDestination
  where
  toPath CreateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v1/sms-voice/configuration-sets/",
        Data.toBS configurationSetName,
        "/event-destinations"
      ]

instance
  Data.ToQuery
    CreateConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An empty object that indicates that the event destination was created
-- successfully.
--
-- /See:/ 'newCreateConfigurationSetEventDestinationResponse' smart constructor.
data CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfigurationSetEventDestinationResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationSetEventDestinationResponse
newCreateConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    CreateConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' CreateConfigurationSetEventDestinationResponse Prelude.Int
createConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\CreateConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetEventDestinationResponse)

instance
  Prelude.NFData
    CreateConfigurationSetEventDestinationResponse
  where
  rnf
    CreateConfigurationSetEventDestinationResponse' {..} =
      Prelude.rnf httpStatus
