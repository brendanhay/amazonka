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
-- Module      : Network.AWS.MediaConvert.DisassociateCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an association between the Amazon Resource Name (ARN) of an AWS
-- Certificate Manager (ACM) certificate and an AWS Elemental MediaConvert
-- resource.
module Network.AWS.MediaConvert.DisassociateCertificate
  ( -- * Creating a Request
    DisassociateCertificate (..),
    newDisassociateCertificate,

    -- * Request Lenses
    disassociateCertificate_arn,

    -- * Destructuring the Response
    DisassociateCertificateResponse (..),
    newDisassociateCertificateResponse,

    -- * Response Lenses
    disassociateCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateCertificate' smart constructor.
data DisassociateCertificate = DisassociateCertificate'
  { -- | The ARN of the ACM certificate that you want to disassociate from your
    -- MediaConvert resource.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociateCertificate_arn' - The ARN of the ACM certificate that you want to disassociate from your
-- MediaConvert resource.
newDisassociateCertificate ::
  -- | 'arn'
  Prelude.Text ->
  DisassociateCertificate
newDisassociateCertificate pArn_ =
  DisassociateCertificate' {arn = pArn_}

-- | The ARN of the ACM certificate that you want to disassociate from your
-- MediaConvert resource.
disassociateCertificate_arn :: Lens.Lens' DisassociateCertificate Prelude.Text
disassociateCertificate_arn = Lens.lens (\DisassociateCertificate' {arn} -> arn) (\s@DisassociateCertificate' {} a -> s {arn = a} :: DisassociateCertificate)

instance Prelude.AWSRequest DisassociateCertificate where
  type
    Rs DisassociateCertificate =
      DisassociateCertificateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateCertificate

instance Prelude.NFData DisassociateCertificate

instance Prelude.ToHeaders DisassociateCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DisassociateCertificate where
  toPath DisassociateCertificate' {..} =
    Prelude.mconcat
      ["/2017-08-29/certificates/", Prelude.toBS arn]

instance Prelude.ToQuery DisassociateCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateCertificateResponse' smart constructor.
data DisassociateCertificateResponse = DisassociateCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateCertificateResponse_httpStatus' - The response's http status code.
newDisassociateCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateCertificateResponse
newDisassociateCertificateResponse pHttpStatus_ =
  DisassociateCertificateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateCertificateResponse_httpStatus :: Lens.Lens' DisassociateCertificateResponse Prelude.Int
disassociateCertificateResponse_httpStatus = Lens.lens (\DisassociateCertificateResponse' {httpStatus} -> httpStatus) (\s@DisassociateCertificateResponse' {} a -> s {httpStatus = a} :: DisassociateCertificateResponse)

instance
  Prelude.NFData
    DisassociateCertificateResponse
