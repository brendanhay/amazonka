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
-- Module      : Amazonka.CertificateManager.UpdateCertificateOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a certificate. Currently, you can use this function to specify
-- whether to opt in to or out of recording your certificate in a
-- certificate transparency log. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging>.
module Amazonka.CertificateManager.UpdateCertificateOptions
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

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCertificateOptions' smart constructor.
data UpdateCertificateOptions = UpdateCertificateOptions'
  { -- | ARN of the requested certificate to update. This must be of the form:
    --
    -- @arn:aws:acm:us-east-1:account:certificate\/12345678-1234-1234-1234-123456789012 @
    certificateArn :: Prelude.Text,
    -- | Use to update the options for your certificate. Currently, you can
    -- specify whether to add your certificate to a transparency log.
    -- Certificate transparency makes it possible to detect SSL\/TLS
    -- certificates that have been mistakenly or maliciously issued.
    -- Certificates that have not been logged typically produce an error
    -- message in a browser.
    options :: CertificateOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
updateCertificateOptions_certificateArn :: Lens.Lens' UpdateCertificateOptions Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateCertificateOptionsResponse'

instance Prelude.Hashable UpdateCertificateOptions where
  hashWithSalt _salt UpdateCertificateOptions' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` options

instance Prelude.NFData UpdateCertificateOptions where
  rnf UpdateCertificateOptions' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf options

instance Core.ToHeaders UpdateCertificateOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.UpdateCertificateOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCertificateOptions where
  toJSON UpdateCertificateOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn),
            Prelude.Just ("Options" Core..= options)
          ]
      )

instance Core.ToPath UpdateCertificateOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateCertificateOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCertificateOptionsResponse' smart constructor.
data UpdateCertificateOptionsResponse = UpdateCertificateOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificateOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCertificateOptionsResponse ::
  UpdateCertificateOptionsResponse
newUpdateCertificateOptionsResponse =
  UpdateCertificateOptionsResponse'

instance
  Prelude.NFData
    UpdateCertificateOptionsResponse
  where
  rnf _ = ()
