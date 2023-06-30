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
-- Module      : Amazonka.SES.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the delivery options for a configuration set.
module Amazonka.SES.PutConfigurationSetDeliveryOptions
  ( -- * Creating a Request
    PutConfigurationSetDeliveryOptions (..),
    newPutConfigurationSetDeliveryOptions,

    -- * Request Lenses
    putConfigurationSetDeliveryOptions_deliveryOptions,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | A request to modify the delivery options for a configuration set.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS).
    deliveryOptions :: Prelude.Maybe DeliveryOptions,
    -- | The name of the configuration set that you want to specify the delivery
    -- options for.
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
-- 'deliveryOptions', 'putConfigurationSetDeliveryOptions_deliveryOptions' - Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS).
--
-- 'configurationSetName', 'putConfigurationSetDeliveryOptions_configurationSetName' - The name of the configuration set that you want to specify the delivery
-- options for.
newPutConfigurationSetDeliveryOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetDeliveryOptions
newPutConfigurationSetDeliveryOptions
  pConfigurationSetName_ =
    PutConfigurationSetDeliveryOptions'
      { deliveryOptions =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS).
putConfigurationSetDeliveryOptions_deliveryOptions :: Lens.Lens' PutConfigurationSetDeliveryOptions (Prelude.Maybe DeliveryOptions)
putConfigurationSetDeliveryOptions_deliveryOptions = Lens.lens (\PutConfigurationSetDeliveryOptions' {deliveryOptions} -> deliveryOptions) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {deliveryOptions = a} :: PutConfigurationSetDeliveryOptions)

-- | The name of the configuration set that you want to specify the delivery
-- options for.
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
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutConfigurationSetDeliveryOptionsResult"
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
      _salt
        `Prelude.hashWithSalt` deliveryOptions
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    PutConfigurationSetDeliveryOptions
  where
  rnf PutConfigurationSetDeliveryOptions' {..} =
    Prelude.rnf deliveryOptions
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    PutConfigurationSetDeliveryOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    PutConfigurationSetDeliveryOptions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PutConfigurationSetDeliveryOptions
  where
  toQuery PutConfigurationSetDeliveryOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PutConfigurationSetDeliveryOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "DeliveryOptions" Data.=: deliveryOptions,
        "ConfigurationSetName" Data.=: configurationSetName
      ]

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
