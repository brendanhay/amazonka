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
-- Module      : Amazonka.SES.DeleteConfigurationSetTrackingOptions
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
module Amazonka.SES.DeleteConfigurationSetTrackingOptions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete open and click tracking options in a
-- configuration set.
--
-- /See:/ 'newDeleteConfigurationSetTrackingOptions' smart constructor.
data DeleteConfigurationSetTrackingOptions = DeleteConfigurationSetTrackingOptions'
  { -- | The name of the configuration set from which you want to delete the
    -- tracking options.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteConfigurationSetTrackingOptions
newDeleteConfigurationSetTrackingOptions
  pConfigurationSetName_ =
    DeleteConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_
      }

-- | The name of the configuration set from which you want to delete the
-- tracking options.
deleteConfigurationSetTrackingOptions_configurationSetName :: Lens.Lens' DeleteConfigurationSetTrackingOptions Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteConfigurationSetTrackingOptions
  where
  hashWithSalt
    _salt
    DeleteConfigurationSetTrackingOptions' {..} =
      _salt `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    DeleteConfigurationSetTrackingOptions
  where
  rnf DeleteConfigurationSetTrackingOptions' {..} =
    Prelude.rnf configurationSetName

instance
  Core.ToHeaders
    DeleteConfigurationSetTrackingOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteConfigurationSetTrackingOptions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteConfigurationSetTrackingOptions
  where
  toQuery DeleteConfigurationSetTrackingOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteConfigurationSetTrackingOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteConfigurationSetTrackingOptionsResponse' smart constructor.
data DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteConfigurationSetTrackingOptionsResponse
newDeleteConfigurationSetTrackingOptionsResponse
  pHttpStatus_ =
    DeleteConfigurationSetTrackingOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfigurationSetTrackingOptionsResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetTrackingOptionsResponse Prelude.Int
deleteConfigurationSetTrackingOptionsResponse_httpStatus = Lens.lens (\DeleteConfigurationSetTrackingOptionsResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetTrackingOptionsResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetTrackingOptionsResponse)

instance
  Prelude.NFData
    DeleteConfigurationSetTrackingOptionsResponse
  where
  rnf
    DeleteConfigurationSetTrackingOptionsResponse' {..} =
      Prelude.rnf httpStatus
