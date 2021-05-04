{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the delivery options for a configuration set.
module Network.AWS.SES.PutConfigurationSetDeliveryOptions
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    PutConfigurationSetDeliveryOptions
  where
  type
    Rs PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request = Request.postQuery defaultService
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

instance
  Prelude.NFData
    PutConfigurationSetDeliveryOptions

instance
  Prelude.ToHeaders
    PutConfigurationSetDeliveryOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    PutConfigurationSetDeliveryOptions
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    PutConfigurationSetDeliveryOptions
  where
  toQuery PutConfigurationSetDeliveryOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "PutConfigurationSetDeliveryOptions" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "DeliveryOptions" Prelude.=: deliveryOptions,
        "ConfigurationSetName"
          Prelude.=: configurationSetName
      ]

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptionsResponse' smart constructor.
data PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
