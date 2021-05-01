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
-- Module      : Network.AWS.SES.CreateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a configuration set and a custom domain
-- for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are
-- hosted on domains operated by Amazon SES. You can configure a subdomain
-- of your own to handle these events. For information about using custom
-- domains, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide>.
module Network.AWS.SES.CreateConfigurationSetTrackingOptions
  ( -- * Creating a Request
    CreateConfigurationSetTrackingOptions (..),
    newCreateConfigurationSetTrackingOptions,

    -- * Request Lenses
    createConfigurationSetTrackingOptions_configurationSetName,
    createConfigurationSetTrackingOptions_trackingOptions,

    -- * Destructuring the Response
    CreateConfigurationSetTrackingOptionsResponse (..),
    newCreateConfigurationSetTrackingOptionsResponse,

    -- * Response Lenses
    createConfigurationSetTrackingOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to create an open and click tracking option object
-- in a configuration set.
--
-- /See:/ 'newCreateConfigurationSetTrackingOptions' smart constructor.
data CreateConfigurationSetTrackingOptions = CreateConfigurationSetTrackingOptions'
  { -- | The name of the configuration set that the tracking options should be
    -- associated with.
    configurationSetName :: Prelude.Text,
    trackingOptions :: TrackingOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetTrackingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'createConfigurationSetTrackingOptions_configurationSetName' - The name of the configuration set that the tracking options should be
-- associated with.
--
-- 'trackingOptions', 'createConfigurationSetTrackingOptions_trackingOptions' - Undocumented member.
newCreateConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'trackingOptions'
  TrackingOptions ->
  CreateConfigurationSetTrackingOptions
newCreateConfigurationSetTrackingOptions
  pConfigurationSetName_
  pTrackingOptions_ =
    CreateConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_,
        trackingOptions = pTrackingOptions_
      }

-- | The name of the configuration set that the tracking options should be
-- associated with.
createConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' CreateConfigurationSetTrackingOptions Prelude.Text
createConfigurationSetTrackingOptions_configurationSetName = Lens.lens (\CreateConfigurationSetTrackingOptions' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSetTrackingOptions' {} a -> s {configurationSetName = a} :: CreateConfigurationSetTrackingOptions)

-- | Undocumented member.
createConfigurationSetTrackingOptions_trackingOptions :: Lens.Lens' CreateConfigurationSetTrackingOptions TrackingOptions
createConfigurationSetTrackingOptions_trackingOptions = Lens.lens (\CreateConfigurationSetTrackingOptions' {trackingOptions} -> trackingOptions) (\s@CreateConfigurationSetTrackingOptions' {} a -> s {trackingOptions = a} :: CreateConfigurationSetTrackingOptions)

instance
  Prelude.AWSRequest
    CreateConfigurationSetTrackingOptions
  where
  type
    Rs CreateConfigurationSetTrackingOptions =
      CreateConfigurationSetTrackingOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          CreateConfigurationSetTrackingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConfigurationSetTrackingOptions

instance
  Prelude.NFData
    CreateConfigurationSetTrackingOptions

instance
  Prelude.ToHeaders
    CreateConfigurationSetTrackingOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    CreateConfigurationSetTrackingOptions
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateConfigurationSetTrackingOptions
  where
  toQuery CreateConfigurationSetTrackingOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateConfigurationSetTrackingOptions" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "TrackingOptions" Prelude.=: trackingOptions
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateConfigurationSetTrackingOptionsResponse' smart constructor.
data CreateConfigurationSetTrackingOptionsResponse = CreateConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetTrackingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfigurationSetTrackingOptionsResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetTrackingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationSetTrackingOptionsResponse
newCreateConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    CreateConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' CreateConfigurationSetTrackingOptionsResponse Prelude.Int
createConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\CreateConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetTrackingOptionsResponse)

instance
  Prelude.NFData
    CreateConfigurationSetTrackingOptionsResponse
