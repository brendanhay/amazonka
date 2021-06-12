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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | A request to modify the delivery options for a configuration set.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS).
    deliveryOptions :: Core.Maybe DeliveryOptions,
    -- | The name of the configuration set that you want to specify the delivery
    -- options for.
    configurationSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  PutConfigurationSetDeliveryOptions
newPutConfigurationSetDeliveryOptions
  pConfigurationSetName_ =
    PutConfigurationSetDeliveryOptions'
      { deliveryOptions =
          Core.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS).
putConfigurationSetDeliveryOptions_deliveryOptions :: Lens.Lens' PutConfigurationSetDeliveryOptions (Core.Maybe DeliveryOptions)
putConfigurationSetDeliveryOptions_deliveryOptions = Lens.lens (\PutConfigurationSetDeliveryOptions' {deliveryOptions} -> deliveryOptions) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {deliveryOptions = a} :: PutConfigurationSetDeliveryOptions)

-- | The name of the configuration set that you want to specify the delivery
-- options for.
putConfigurationSetDeliveryOptions_configurationSetName :: Lens.Lens' PutConfigurationSetDeliveryOptions Core.Text
putConfigurationSetDeliveryOptions_configurationSetName = Lens.lens (\PutConfigurationSetDeliveryOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetDeliveryOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetDeliveryOptions)

instance
  Core.AWSRequest
    PutConfigurationSetDeliveryOptions
  where
  type
    AWSResponse PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PutConfigurationSetDeliveryOptionsResult"
      ( \s h x ->
          PutConfigurationSetDeliveryOptionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    PutConfigurationSetDeliveryOptions

instance
  Core.NFData
    PutConfigurationSetDeliveryOptions

instance
  Core.ToHeaders
    PutConfigurationSetDeliveryOptions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    PutConfigurationSetDeliveryOptions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    PutConfigurationSetDeliveryOptions
  where
  toQuery PutConfigurationSetDeliveryOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "PutConfigurationSetDeliveryOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "DeliveryOptions" Core.=: deliveryOptions,
        "ConfigurationSetName" Core.=: configurationSetName
      ]

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetDeliveryOptionsResponse' smart constructor.
data PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutConfigurationSetDeliveryOptionsResponse
newPutConfigurationSetDeliveryOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetDeliveryOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetDeliveryOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetDeliveryOptionsResponse Core.Int
putConfigurationSetDeliveryOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetDeliveryOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetDeliveryOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetDeliveryOptionsResponse)

instance
  Core.NFData
    PutConfigurationSetDeliveryOptionsResponse
