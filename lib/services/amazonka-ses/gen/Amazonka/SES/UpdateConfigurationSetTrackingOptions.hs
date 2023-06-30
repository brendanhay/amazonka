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
-- Module      : Amazonka.SES.UpdateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an association between a configuration set and a custom domain
-- for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are
-- hosted on domains operated by Amazon SES. You can configure a subdomain
-- of your own to handle these events. For information about using custom
-- domains, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide>.
module Amazonka.SES.UpdateConfigurationSetTrackingOptions
  ( -- * Creating a Request
    UpdateConfigurationSetTrackingOptions (..),
    newUpdateConfigurationSetTrackingOptions,

    -- * Request Lenses
    updateConfigurationSetTrackingOptions_configurationSetName,
    updateConfigurationSetTrackingOptions_trackingOptions,

    -- * Destructuring the Response
    UpdateConfigurationSetTrackingOptionsResponse (..),
    newUpdateConfigurationSetTrackingOptionsResponse,

    -- * Response Lenses
    updateConfigurationSetTrackingOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to update the tracking options for a configuration
-- set.
--
-- /See:/ 'newUpdateConfigurationSetTrackingOptions' smart constructor.
data UpdateConfigurationSetTrackingOptions = UpdateConfigurationSetTrackingOptions'
  { -- | The name of the configuration set for which you want to update the
    -- custom tracking domain.
    configurationSetName :: Prelude.Text,
    trackingOptions :: TrackingOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetTrackingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'updateConfigurationSetTrackingOptions_configurationSetName' - The name of the configuration set for which you want to update the
-- custom tracking domain.
--
-- 'trackingOptions', 'updateConfigurationSetTrackingOptions_trackingOptions' - Undocumented member.
newUpdateConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'trackingOptions'
  TrackingOptions ->
  UpdateConfigurationSetTrackingOptions
newUpdateConfigurationSetTrackingOptions
  pConfigurationSetName_
  pTrackingOptions_ =
    UpdateConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_,
        trackingOptions = pTrackingOptions_
      }

-- | The name of the configuration set for which you want to update the
-- custom tracking domain.
updateConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' UpdateConfigurationSetTrackingOptions Prelude.Text
updateConfigurationSetTrackingOptions_configurationSetName = Lens.lens (\UpdateConfigurationSetTrackingOptions' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetTrackingOptions' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetTrackingOptions)

-- | Undocumented member.
updateConfigurationSetTrackingOptions_trackingOptions :: Lens.Lens' UpdateConfigurationSetTrackingOptions TrackingOptions
updateConfigurationSetTrackingOptions_trackingOptions = Lens.lens (\UpdateConfigurationSetTrackingOptions' {trackingOptions} -> trackingOptions) (\s@UpdateConfigurationSetTrackingOptions' {} a -> s {trackingOptions = a} :: UpdateConfigurationSetTrackingOptions)

instance
  Core.AWSRequest
    UpdateConfigurationSetTrackingOptions
  where
  type
    AWSResponse
      UpdateConfigurationSetTrackingOptions =
      UpdateConfigurationSetTrackingOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          UpdateConfigurationSetTrackingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateConfigurationSetTrackingOptions
  where
  hashWithSalt
    _salt
    UpdateConfigurationSetTrackingOptions' {..} =
      _salt
        `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` trackingOptions

instance
  Prelude.NFData
    UpdateConfigurationSetTrackingOptions
  where
  rnf UpdateConfigurationSetTrackingOptions' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf trackingOptions

instance
  Data.ToHeaders
    UpdateConfigurationSetTrackingOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    UpdateConfigurationSetTrackingOptions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateConfigurationSetTrackingOptions
  where
  toQuery UpdateConfigurationSetTrackingOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UpdateConfigurationSetTrackingOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Data.=: configurationSetName,
        "TrackingOptions" Data.=: trackingOptions
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newUpdateConfigurationSetTrackingOptionsResponse' smart constructor.
data UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetTrackingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfigurationSetTrackingOptionsResponse_httpStatus' - The response's http status code.
newUpdateConfigurationSetTrackingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConfigurationSetTrackingOptionsResponse
newUpdateConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    UpdateConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' UpdateConfigurationSetTrackingOptionsResponse Prelude.Int
updateConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\UpdateConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationSetTrackingOptionsResponse)

instance
  Prelude.NFData
    UpdateConfigurationSetTrackingOptionsResponse
  where
  rnf
    UpdateConfigurationSetTrackingOptionsResponse' {..} =
      Prelude.rnf httpStatus
