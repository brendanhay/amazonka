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
-- Module      : Network.AWS.SES.UpdateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.UpdateConfigurationSetTrackingOptions
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to update the tracking options for a configuration
-- set.
--
-- /See:/ 'newUpdateConfigurationSetTrackingOptions' smart constructor.
data UpdateConfigurationSetTrackingOptions = UpdateConfigurationSetTrackingOptions'
  { -- | The name of the configuration set for which you want to update the
    -- custom tracking domain.
    configurationSetName :: Core.Text,
    trackingOptions :: TrackingOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
updateConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' UpdateConfigurationSetTrackingOptions Core.Text
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          UpdateConfigurationSetTrackingOptionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateConfigurationSetTrackingOptions

instance
  Core.NFData
    UpdateConfigurationSetTrackingOptions

instance
  Core.ToHeaders
    UpdateConfigurationSetTrackingOptions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    UpdateConfigurationSetTrackingOptions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateConfigurationSetTrackingOptions
  where
  toQuery UpdateConfigurationSetTrackingOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "UpdateConfigurationSetTrackingOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName,
        "TrackingOptions" Core.=: trackingOptions
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newUpdateConfigurationSetTrackingOptionsResponse' smart constructor.
data UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateConfigurationSetTrackingOptionsResponse
newUpdateConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    UpdateConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' UpdateConfigurationSetTrackingOptionsResponse Core.Int
updateConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\UpdateConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationSetTrackingOptionsResponse)

instance
  Core.NFData
    UpdateConfigurationSetTrackingOptionsResponse
