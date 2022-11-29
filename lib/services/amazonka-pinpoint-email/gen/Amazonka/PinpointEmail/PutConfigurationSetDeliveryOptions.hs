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
-- Module      : Amazonka.PinpointEmail.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a configuration set with a dedicated IP pool. You can use
-- dedicated IP pools to create groups of dedicated IP addresses for
-- sending specific types of email.
module Amazonka.PinpointEmail.PutConfigurationSetDeliveryOptions
  ( -- * Creating a Request
    PutConfigurationSetDeliveryOptions (..),
    newPutConfigurationSetDeliveryOptions,

    -- * Request Lenses
    putConfigurationSetDeliveryOptions_tlsPolicy,
    putConfigurationSetDeliveryOptions_sendingPoolName,
    putConfigurationSetDeliveryOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetDeliveryOptionsResponse (..),
    newPutConfigurationSetDeliveryOptionsResponse,

    -- * Response Lenses
    putConfigurationSetDeliveryOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to associate a configuration set with a dedicated IP pool.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS). If the value is @Require@,
    -- messages are only delivered if a TLS connection can be established. If
    -- the value is @Optional@, messages can be delivered in plain text if a
    -- TLS connection can\'t be established.
    tlsPolicy :: Prelude.Maybe TlsPolicy,
    -- | The name of the dedicated IP pool that you want to associate with the
    -- configuration set.
    sendingPoolName :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set that you want to associate with a
    -- dedicated IP pool.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetDeliveryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tlsPolicy', 'putConfigurationSetDeliveryOptions_tlsPolicy' - Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
--
-- 'sendingPoolName', 'putConfigurationSetDeliveryOptions_sendingPoolName' - The name of the dedicated IP pool that you want to associate with the
-- configuration set.
--
-- 'configurationSetName', 'putConfigurationSetDeliveryOptions_configurationSetName' - The name of the configuration set that you want to associate with a
-- dedicated IP pool.
newPutConfigurationSetDeliveryOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetDeliveryOptions
newPutConfigurationSetDeliveryOptions
  pConfigurationSetName_ =
    PutConfigurationSetDeliveryOptions'
      { tlsPolicy =
          Prelude.Nothing,
        sendingPoolName = Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
putConfigurationSetDeliveryOptions_tlsPolicy :: Lens.Lens' PutConfigurationSetDeliveryOptions (Prelude.Maybe TlsPolicy)
putConfigurationSetDeliveryOptions_tlsPolicy = Lens.lens (\PutConfigurationSetDeliveryOptions' {tlsPolicy} -> tlsPolicy) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {tlsPolicy = a} :: PutConfigurationSetDeliveryOptions)

-- | The name of the dedicated IP pool that you want to associate with the
-- configuration set.
putConfigurationSetDeliveryOptions_sendingPoolName :: Lens.Lens' PutConfigurationSetDeliveryOptions (Prelude.Maybe Prelude.Text)
putConfigurationSetDeliveryOptions_sendingPoolName = Lens.lens (\PutConfigurationSetDeliveryOptions' {sendingPoolName} -> sendingPoolName) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {sendingPoolName = a} :: PutConfigurationSetDeliveryOptions)

-- | The name of the configuration set that you want to associate with a
-- dedicated IP pool.
putConfigurationSetDeliveryOptions_configurationSetName :: Lens.Lens' PutConfigurationSetDeliveryOptions Prelude.Text
putConfigurationSetDeliveryOptions_configurationSetName = Lens.lens (\PutConfigurationSetDeliveryOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetDeliveryOptions)

instance
  Core.AWSRequest
    PutConfigurationSetDeliveryOptions
  where
  type
    AWSResponse PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetDeliveryOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetDeliveryOptions
  where
  hashWithSalt
    _salt
    PutConfigurationSetDeliveryOptions' {..} =
      _salt `Prelude.hashWithSalt` tlsPolicy
        `Prelude.hashWithSalt` sendingPoolName
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    PutConfigurationSetDeliveryOptions
  where
  rnf PutConfigurationSetDeliveryOptions' {..} =
    Prelude.rnf tlsPolicy
      `Prelude.seq` Prelude.rnf sendingPoolName
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Core.ToHeaders
    PutConfigurationSetDeliveryOptions
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
    PutConfigurationSetDeliveryOptions
  where
  toJSON PutConfigurationSetDeliveryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TlsPolicy" Core..=) Prelude.<$> tlsPolicy,
            ("SendingPoolName" Core..=)
              Prelude.<$> sendingPoolName
          ]
      )

instance
  Core.ToPath
    PutConfigurationSetDeliveryOptions
  where
  toPath PutConfigurationSetDeliveryOptions' {..} =
    Prelude.mconcat
      [ "/v1/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/delivery-options"
      ]

instance
  Core.ToQuery
    PutConfigurationSetDeliveryOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptionsResponse' smart constructor.
data PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetDeliveryOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetDeliveryOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetDeliveryOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetDeliveryOptionsResponse
newPutConfigurationSetDeliveryOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetDeliveryOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetDeliveryOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetDeliveryOptionsResponse Prelude.Int
putConfigurationSetDeliveryOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetDeliveryOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetDeliveryOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetDeliveryOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetDeliveryOptionsResponse
  where
  rnf PutConfigurationSetDeliveryOptionsResponse' {..} =
    Prelude.rnf httpStatus
