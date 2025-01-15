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
-- Module      : Amazonka.SESV2.PutConfigurationSetReputationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable collection of reputation metrics for emails that you
-- send using a particular configuration set in a specific Amazon Web
-- Services Region.
module Amazonka.SESV2.PutConfigurationSetReputationOptions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to enable or disable tracking of reputation metrics for a
-- configuration set.
--
-- /See:/ 'newPutConfigurationSetReputationOptions' smart constructor.
data PutConfigurationSetReputationOptions = PutConfigurationSetReputationOptions'
  { -- | If @true@, tracking of reputation metrics is enabled for the
    -- configuration set. If @false@, tracking of reputation metrics is
    -- disabled for the configuration set.
    reputationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the configuration set.
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
-- 'configurationSetName', 'putConfigurationSetReputationOptions_configurationSetName' - The name of the configuration set.
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

-- | The name of the configuration set.
putConfigurationSetReputationOptions_configurationSetName :: Lens.Lens' PutConfigurationSetReputationOptions Prelude.Text
putConfigurationSetReputationOptions_configurationSetName = Lens.lens (\PutConfigurationSetReputationOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetReputationOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetReputationOptions)

instance
  Core.AWSRequest
    PutConfigurationSetReputationOptions
  where
  type
    AWSResponse PutConfigurationSetReputationOptions =
      PutConfigurationSetReputationOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetReputationOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetReputationOptions
  where
  hashWithSalt
    _salt
    PutConfigurationSetReputationOptions' {..} =
      _salt
        `Prelude.hashWithSalt` reputationMetricsEnabled
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    PutConfigurationSetReputationOptions
  where
  rnf PutConfigurationSetReputationOptions' {..} =
    Prelude.rnf reputationMetricsEnabled `Prelude.seq`
      Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    PutConfigurationSetReputationOptions
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
    PutConfigurationSetReputationOptions
  where
  toJSON PutConfigurationSetReputationOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReputationMetricsEnabled" Data..=)
              Prelude.<$> reputationMetricsEnabled
          ]
      )

instance
  Data.ToPath
    PutConfigurationSetReputationOptions
  where
  toPath PutConfigurationSetReputationOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Data.toBS configurationSetName,
        "/reputation-options"
      ]

instance
  Data.ToQuery
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
  where
  rnf PutConfigurationSetReputationOptionsResponse' {..} =
    Prelude.rnf httpStatus
