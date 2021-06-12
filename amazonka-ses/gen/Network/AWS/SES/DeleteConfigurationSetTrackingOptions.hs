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
-- Module      : Network.AWS.SES.DeleteConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association between a configuration set and a custom domain
-- for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are
-- hosted on domains operated by Amazon SES. You can configure a subdomain
-- of your own to handle these events. For information about using custom
-- domains, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide>.
--
-- Deleting this kind of association will result in emails sent using the
-- specified configuration set to capture open and click events using the
-- standard, Amazon SES-operated domains.
module Network.AWS.SES.DeleteConfigurationSetTrackingOptions
  ( -- * Creating a Request
    DeleteConfigurationSetTrackingOptions (..),
    newDeleteConfigurationSetTrackingOptions,

    -- * Request Lenses
    deleteConfigurationSetTrackingOptions_configurationSetName,

    -- * Destructuring the Response
    DeleteConfigurationSetTrackingOptionsResponse (..),
    newDeleteConfigurationSetTrackingOptionsResponse,

    -- * Response Lenses
    deleteConfigurationSetTrackingOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete open and click tracking options in a
-- configuration set.
--
-- /See:/ 'newDeleteConfigurationSetTrackingOptions' smart constructor.
data DeleteConfigurationSetTrackingOptions = DeleteConfigurationSetTrackingOptions'
  { -- | The name of the configuration set from which you want to delete the
    -- tracking options.
    configurationSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetTrackingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteConfigurationSetTrackingOptions_configurationSetName' - The name of the configuration set from which you want to delete the
-- tracking options.
newDeleteConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Core.Text ->
  DeleteConfigurationSetTrackingOptions
newDeleteConfigurationSetTrackingOptions
  pConfigurationSetName_ =
    DeleteConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_
      }

-- | The name of the configuration set from which you want to delete the
-- tracking options.
deleteConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' DeleteConfigurationSetTrackingOptions Core.Text
deleteConfigurationSetTrackingOptions_configurationSetName = Lens.lens (\DeleteConfigurationSetTrackingOptions' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetTrackingOptions' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetTrackingOptions)

instance
  Core.AWSRequest
    DeleteConfigurationSetTrackingOptions
  where
  type
    AWSResponse
      DeleteConfigurationSetTrackingOptions =
      DeleteConfigurationSetTrackingOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          DeleteConfigurationSetTrackingOptionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteConfigurationSetTrackingOptions

instance
  Core.NFData
    DeleteConfigurationSetTrackingOptions

instance
  Core.ToHeaders
    DeleteConfigurationSetTrackingOptions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteConfigurationSetTrackingOptions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteConfigurationSetTrackingOptions
  where
  toQuery DeleteConfigurationSetTrackingOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteConfigurationSetTrackingOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteConfigurationSetTrackingOptionsResponse' smart constructor.
data DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetTrackingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfigurationSetTrackingOptionsResponse_httpStatus' - The response's http status code.
newDeleteConfigurationSetTrackingOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteConfigurationSetTrackingOptionsResponse
newDeleteConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    DeleteConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetTrackingOptionsResponse Core.Int
deleteConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\DeleteConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetTrackingOptionsResponse)

instance
  Core.NFData
    DeleteConfigurationSetTrackingOptionsResponse
