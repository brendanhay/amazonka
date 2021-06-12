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
-- Module      : Network.AWS.CertificateManager.UpdateCertificateOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a certificate. Currently, you can use this function to specify
-- whether to opt in to or out of recording your certificate in a
-- certificate transparency log. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging>.
module Network.AWS.CertificateManager.UpdateCertificateOptions
  ( -- * Creating a Request
    UpdateCertificateOptions (..),
    newUpdateCertificateOptions,

    -- * Request Lenses
    updateCertificateOptions_certificateArn,
    updateCertificateOptions_options,

    -- * Destructuring the Response
    UpdateCertificateOptionsResponse (..),
    newUpdateCertificateOptionsResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCertificateOptions' smart constructor.
data UpdateCertificateOptions = UpdateCertificateOptions'
  { -- | ARN of the requested certificate to update. This must be of the form:
    --
    -- @arn:aws:acm:us-east-1:account:certificate\/12345678-1234-1234-1234-123456789012 @
    certificateArn :: Core.Text,
    -- | Use to update the options for your certificate. Currently, you can
    -- specify whether to add your certificate to a transparency log.
    -- Certificate transparency makes it possible to detect SSL\/TLS
    -- certificates that have been mistakenly or maliciously issued.
    -- Certificates that have not been logged typically produce an error
    -- message in a browser.
    options :: CertificateOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCertificateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'updateCertificateOptions_certificateArn' - ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:account:certificate\/12345678-1234-1234-1234-123456789012 @
--
-- 'options', 'updateCertificateOptions_options' - Use to update the options for your certificate. Currently, you can
-- specify whether to add your certificate to a transparency log.
-- Certificate transparency makes it possible to detect SSL\/TLS
-- certificates that have been mistakenly or maliciously issued.
-- Certificates that have not been logged typically produce an error
-- message in a browser.
newUpdateCertificateOptions ::
  -- | 'certificateArn'
  Core.Text ->
  -- | 'options'
  CertificateOptions ->
  UpdateCertificateOptions
newUpdateCertificateOptions
  pCertificateArn_
  pOptions_ =
    UpdateCertificateOptions'
      { certificateArn =
          pCertificateArn_,
        options = pOptions_
      }

-- | ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:account:certificate\/12345678-1234-1234-1234-123456789012 @
updateCertificateOptions_certificateArn :: Lens.Lens' UpdateCertificateOptions Core.Text
updateCertificateOptions_certificateArn = Lens.lens (\UpdateCertificateOptions' {certificateArn} -> certificateArn) (\s@UpdateCertificateOptions' {} a -> s {certificateArn = a} :: UpdateCertificateOptions)

-- | Use to update the options for your certificate. Currently, you can
-- specify whether to add your certificate to a transparency log.
-- Certificate transparency makes it possible to detect SSL\/TLS
-- certificates that have been mistakenly or maliciously issued.
-- Certificates that have not been logged typically produce an error
-- message in a browser.
updateCertificateOptions_options :: Lens.Lens' UpdateCertificateOptions CertificateOptions
updateCertificateOptions_options = Lens.lens (\UpdateCertificateOptions' {options} -> options) (\s@UpdateCertificateOptions' {} a -> s {options = a} :: UpdateCertificateOptions)

instance Core.AWSRequest UpdateCertificateOptions where
  type
    AWSResponse UpdateCertificateOptions =
      UpdateCertificateOptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateCertificateOptionsResponse'

instance Core.Hashable UpdateCertificateOptions

instance Core.NFData UpdateCertificateOptions

instance Core.ToHeaders UpdateCertificateOptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.UpdateCertificateOptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCertificateOptions where
  toJSON UpdateCertificateOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CertificateArn" Core..= certificateArn),
            Core.Just ("Options" Core..= options)
          ]
      )

instance Core.ToPath UpdateCertificateOptions where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCertificateOptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCertificateOptionsResponse' smart constructor.
data UpdateCertificateOptionsResponse = UpdateCertificateOptionsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCertificateOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCertificateOptionsResponse ::
  UpdateCertificateOptionsResponse
newUpdateCertificateOptionsResponse =
  UpdateCertificateOptionsResponse'

instance Core.NFData UpdateCertificateOptionsResponse
