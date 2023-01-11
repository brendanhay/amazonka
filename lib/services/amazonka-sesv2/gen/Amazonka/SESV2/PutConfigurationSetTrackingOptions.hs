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
-- Module      : Amazonka.SESV2.PutConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify a custom domain to use for open and click tracking elements in
-- email that you send.
module Amazonka.SESV2.PutConfigurationSetTrackingOptions
  ( -- * Creating a Request
    PutConfigurationSetTrackingOptions (..),
    newPutConfigurationSetTrackingOptions,

    -- * Request Lenses
    putConfigurationSetTrackingOptions_customRedirectDomain,
    putConfigurationSetTrackingOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetTrackingOptionsResponse (..),
    newPutConfigurationSetTrackingOptionsResponse,

    -- * Response Lenses
    putConfigurationSetTrackingOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to add a custom domain for tracking open and click events to a
-- configuration set.
--
-- /See:/ 'newPutConfigurationSetTrackingOptions' smart constructor.
data PutConfigurationSetTrackingOptions = PutConfigurationSetTrackingOptions'
  { -- | The domain to use to track open and click events.
    customRedirectDomain :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetTrackingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRedirectDomain', 'putConfigurationSetTrackingOptions_customRedirectDomain' - The domain to use to track open and click events.
--
-- 'configurationSetName', 'putConfigurationSetTrackingOptions_configurationSetName' - The name of the configuration set.
newPutConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetTrackingOptions
newPutConfigurationSetTrackingOptions
  pConfigurationSetName_ =
    PutConfigurationSetTrackingOptions'
      { customRedirectDomain =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | The domain to use to track open and click events.
putConfigurationSetTrackingOptions_customRedirectDomain :: Lens.Lens' PutConfigurationSetTrackingOptions (Prelude.Maybe Prelude.Text)
putConfigurationSetTrackingOptions_customRedirectDomain = Lens.lens (\PutConfigurationSetTrackingOptions' {customRedirectDomain} -> customRedirectDomain) (\s@PutConfigurationSetTrackingOptions' {} a -> s {customRedirectDomain = a} :: PutConfigurationSetTrackingOptions)

-- | The name of the configuration set.
putConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' PutConfigurationSetTrackingOptions Prelude.Text
putConfigurationSetTrackingOptions_configurationSetName = Lens.lens (\PutConfigurationSetTrackingOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetTrackingOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetTrackingOptions)

instance
  Core.AWSRequest
    PutConfigurationSetTrackingOptions
  where
  type
    AWSResponse PutConfigurationSetTrackingOptions =
      PutConfigurationSetTrackingOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetTrackingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetTrackingOptions
  where
  hashWithSalt
    _salt
    PutConfigurationSetTrackingOptions' {..} =
      _salt `Prelude.hashWithSalt` customRedirectDomain
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    PutConfigurationSetTrackingOptions
  where
  rnf PutConfigurationSetTrackingOptions' {..} =
    Prelude.rnf customRedirectDomain
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    PutConfigurationSetTrackingOptions
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
    PutConfigurationSetTrackingOptions
  where
  toJSON PutConfigurationSetTrackingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRedirectDomain" Data..=)
              Prelude.<$> customRedirectDomain
          ]
      )

instance
  Data.ToPath
    PutConfigurationSetTrackingOptions
  where
  toPath PutConfigurationSetTrackingOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Data.toBS configurationSetName,
        "/tracking-options"
      ]

instance
  Data.ToQuery
    PutConfigurationSetTrackingOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetTrackingOptionsResponse' smart constructor.
data PutConfigurationSetTrackingOptionsResponse = PutConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetTrackingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetTrackingOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetTrackingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetTrackingOptionsResponse
newPutConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetTrackingOptionsResponse Prelude.Int
putConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetTrackingOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetTrackingOptionsResponse
  where
  rnf PutConfigurationSetTrackingOptionsResponse' {..} =
    Prelude.rnf httpStatus
