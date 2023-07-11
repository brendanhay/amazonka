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
-- Module      : Amazonka.MediaConvert.DisassociateCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an association between the Amazon Resource Name (ARN) of an AWS
-- Certificate Manager (ACM) certificate and an AWS Elemental MediaConvert
-- resource.
module Amazonka.MediaConvert.DisassociateCertificate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateCertificate' smart constructor.
data DisassociateCertificate = DisassociateCertificate'
  { -- | The ARN of the ACM certificate that you want to disassociate from your
    -- MediaConvert resource.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisassociateCertificate where
  type
    AWSResponse DisassociateCertificate =
      DisassociateCertificateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateCertificate where
  hashWithSalt _salt DisassociateCertificate' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DisassociateCertificate where
  rnf DisassociateCertificate' {..} = Prelude.rnf arn

instance Data.ToHeaders DisassociateCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateCertificate where
  toPath DisassociateCertificate' {..} =
    Prelude.mconcat
      ["/2017-08-29/certificates/", Data.toBS arn]

instance Data.ToQuery DisassociateCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateCertificateResponse' smart constructor.
data DisassociateCertificateResponse = DisassociateCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateCertificateResponse' {..} =
    Prelude.rnf httpStatus
