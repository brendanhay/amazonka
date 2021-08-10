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
-- Module      : Network.AWS.SESv2.PutConfigurationSetReputationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable collection of reputation metrics for emails that you
-- send using a particular configuration set in a specific AWS Region.
module Network.AWS.SESv2.PutConfigurationSetReputationOptions
  ( -- * Creating a Request
    PutConfigurationSetReputationOptions (..),
    newPutConfigurationSetReputationOptions,

    -- * Request Lenses
    putConfigurationSetReputationOptions_reputationMetricsEnabled,
    putConfigurationSetReputationOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetReputationOptionsResponse (..),
    newPutConfigurationSetReputationOptionsResponse,

    -- * Response Lenses
    putConfigurationSetReputationOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to enable or disable tracking of reputation metrics for a
-- configuration set.
--
-- /See:/ 'newPutConfigurationSetReputationOptions' smart constructor.
data PutConfigurationSetReputationOptions = PutConfigurationSetReputationOptions'
  { -- | If @true@, tracking of reputation metrics is enabled for the
    -- configuration set. If @false@, tracking of reputation metrics is
    -- disabled for the configuration set.
    reputationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the configuration set that you want to enable or disable
    -- reputation metric tracking for.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetReputationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reputationMetricsEnabled', 'putConfigurationSetReputationOptions_reputationMetricsEnabled' - If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
--
-- 'configurationSetName', 'putConfigurationSetReputationOptions_configurationSetName' - The name of the configuration set that you want to enable or disable
-- reputation metric tracking for.
newPutConfigurationSetReputationOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetReputationOptions
newPutConfigurationSetReputationOptions
  pConfigurationSetName_ =
    PutConfigurationSetReputationOptions'
      { reputationMetricsEnabled =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | If @true@, tracking of reputation metrics is enabled for the
-- configuration set. If @false@, tracking of reputation metrics is
-- disabled for the configuration set.
putConfigurationSetReputationOptions_reputationMetricsEnabled :: Lens.Lens' PutConfigurationSetReputationOptions (Prelude.Maybe Prelude.Bool)
putConfigurationSetReputationOptions_reputationMetricsEnabled = Lens.lens (\PutConfigurationSetReputationOptions' {reputationMetricsEnabled} -> reputationMetricsEnabled) (\s@PutConfigurationSetReputationOptions' {} a -> s {reputationMetricsEnabled = a} :: PutConfigurationSetReputationOptions)

-- | The name of the configuration set that you want to enable or disable
-- reputation metric tracking for.
putConfigurationSetReputationOptions_configurationSetName :: Lens.Lens' PutConfigurationSetReputationOptions Prelude.Text
putConfigurationSetReputationOptions_configurationSetName = Lens.lens (\PutConfigurationSetReputationOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetReputationOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetReputationOptions)

instance
  Core.AWSRequest
    PutConfigurationSetReputationOptions
  where
  type
    AWSResponse PutConfigurationSetReputationOptions =
      PutConfigurationSetReputationOptionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetReputationOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetReputationOptions

instance
  Prelude.NFData
    PutConfigurationSetReputationOptions

instance
  Core.ToHeaders
    PutConfigurationSetReputationOptions
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
    PutConfigurationSetReputationOptions
  where
  toJSON PutConfigurationSetReputationOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ReputationMetricsEnabled" Core..=)
              Prelude.<$> reputationMetricsEnabled
          ]
      )

instance
  Core.ToPath
    PutConfigurationSetReputationOptions
  where
  toPath PutConfigurationSetReputationOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/reputation-options"
      ]

instance
  Core.ToQuery
    PutConfigurationSetReputationOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetReputationOptionsResponse' smart constructor.
data PutConfigurationSetReputationOptionsResponse = PutConfigurationSetReputationOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetReputationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetReputationOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetReputationOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetReputationOptionsResponse
newPutConfigurationSetReputationOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetReputationOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetReputationOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetReputationOptionsResponse Prelude.Int
putConfigurationSetReputationOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetReputationOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetReputationOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetReputationOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetReputationOptionsResponse
