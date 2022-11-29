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
-- Module      : Amazonka.SESV2.PutConfigurationSetSendingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable email sending for messages that use a particular
-- configuration set in a specific Amazon Web Services Region.
module Amazonka.SESV2.PutConfigurationSetSendingOptions
  ( -- * Creating a Request
    PutConfigurationSetSendingOptions (..),
    newPutConfigurationSetSendingOptions,

    -- * Request Lenses
    putConfigurationSetSendingOptions_sendingEnabled,
    putConfigurationSetSendingOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetSendingOptionsResponse (..),
    newPutConfigurationSetSendingOptionsResponse,

    -- * Response Lenses
    putConfigurationSetSendingOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to enable or disable the ability of Amazon SES to send emails
-- that use a specific configuration set.
--
-- /See:/ 'newPutConfigurationSetSendingOptions' smart constructor.
data PutConfigurationSetSendingOptions = PutConfigurationSetSendingOptions'
  { -- | If @true@, email sending is enabled for the configuration set. If
    -- @false@, email sending is disabled for the configuration set.
    sendingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the configuration set to enable or disable email sending
    -- for.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetSendingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendingEnabled', 'putConfigurationSetSendingOptions_sendingEnabled' - If @true@, email sending is enabled for the configuration set. If
-- @false@, email sending is disabled for the configuration set.
--
-- 'configurationSetName', 'putConfigurationSetSendingOptions_configurationSetName' - The name of the configuration set to enable or disable email sending
-- for.
newPutConfigurationSetSendingOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetSendingOptions
newPutConfigurationSetSendingOptions
  pConfigurationSetName_ =
    PutConfigurationSetSendingOptions'
      { sendingEnabled =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | If @true@, email sending is enabled for the configuration set. If
-- @false@, email sending is disabled for the configuration set.
putConfigurationSetSendingOptions_sendingEnabled :: Lens.Lens' PutConfigurationSetSendingOptions (Prelude.Maybe Prelude.Bool)
putConfigurationSetSendingOptions_sendingEnabled = Lens.lens (\PutConfigurationSetSendingOptions' {sendingEnabled} -> sendingEnabled) (\s@PutConfigurationSetSendingOptions' {} a -> s {sendingEnabled = a} :: PutConfigurationSetSendingOptions)

-- | The name of the configuration set to enable or disable email sending
-- for.
putConfigurationSetSendingOptions_configurationSetName :: Lens.Lens' PutConfigurationSetSendingOptions Prelude.Text
putConfigurationSetSendingOptions_configurationSetName = Lens.lens (\PutConfigurationSetSendingOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetSendingOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetSendingOptions)

instance
  Core.AWSRequest
    PutConfigurationSetSendingOptions
  where
  type
    AWSResponse PutConfigurationSetSendingOptions =
      PutConfigurationSetSendingOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetSendingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetSendingOptions
  where
  hashWithSalt
    _salt
    PutConfigurationSetSendingOptions' {..} =
      _salt `Prelude.hashWithSalt` sendingEnabled
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    PutConfigurationSetSendingOptions
  where
  rnf PutConfigurationSetSendingOptions' {..} =
    Prelude.rnf sendingEnabled
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Core.ToHeaders
    PutConfigurationSetSendingOptions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    PutConfigurationSetSendingOptions
  where
  toJSON PutConfigurationSetSendingOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SendingEnabled" Core..=)
              Prelude.<$> sendingEnabled
          ]
      )

instance
  Core.ToPath
    PutConfigurationSetSendingOptions
  where
  toPath PutConfigurationSetSendingOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/sending"
      ]

instance
  Core.ToQuery
    PutConfigurationSetSendingOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetSendingOptionsResponse' smart constructor.
data PutConfigurationSetSendingOptionsResponse = PutConfigurationSetSendingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetSendingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetSendingOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetSendingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetSendingOptionsResponse
newPutConfigurationSetSendingOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetSendingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetSendingOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetSendingOptionsResponse Prelude.Int
putConfigurationSetSendingOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetSendingOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetSendingOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetSendingOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetSendingOptionsResponse
  where
  rnf PutConfigurationSetSendingOptionsResponse' {..} =
    Prelude.rnf httpStatus
