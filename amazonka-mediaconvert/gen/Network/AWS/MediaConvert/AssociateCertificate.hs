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
-- Module      : Network.AWS.MediaConvert.AssociateCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Certificate Manager (ACM) Amazon Resource Name (ARN)
-- with AWS Elemental MediaConvert.
module Network.AWS.MediaConvert.AssociateCertificate
  ( -- * Creating a Request
    AssociateCertificate (..),
    newAssociateCertificate,

    -- * Request Lenses
    associateCertificate_arn,

    -- * Destructuring the Response
    AssociateCertificateResponse (..),
    newAssociateCertificateResponse,

    -- * Response Lenses
    associateCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateCertificate' smart constructor.
data AssociateCertificate = AssociateCertificate'
  { -- | The ARN of the ACM certificate that you want to associate with your
    -- MediaConvert resource.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associateCertificate_arn' - The ARN of the ACM certificate that you want to associate with your
-- MediaConvert resource.
newAssociateCertificate ::
  -- | 'arn'
  Prelude.Text ->
  AssociateCertificate
newAssociateCertificate pArn_ =
  AssociateCertificate' {arn = pArn_}

-- | The ARN of the ACM certificate that you want to associate with your
-- MediaConvert resource.
associateCertificate_arn :: Lens.Lens' AssociateCertificate Prelude.Text
associateCertificate_arn = Lens.lens (\AssociateCertificate' {arn} -> arn) (\s@AssociateCertificate' {} a -> s {arn = a} :: AssociateCertificate)

instance Prelude.AWSRequest AssociateCertificate where
  type
    Rs AssociateCertificate =
      AssociateCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateCertificate

instance Prelude.NFData AssociateCertificate

instance Prelude.ToHeaders AssociateCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateCertificate where
  toJSON AssociateCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath AssociateCertificate where
  toPath = Prelude.const "/2017-08-29/certificates"

instance Prelude.ToQuery AssociateCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateCertificateResponse' smart constructor.
data AssociateCertificateResponse = AssociateCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateCertificateResponse_httpStatus' - The response's http status code.
newAssociateCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateCertificateResponse
newAssociateCertificateResponse pHttpStatus_ =
  AssociateCertificateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateCertificateResponse_httpStatus :: Lens.Lens' AssociateCertificateResponse Prelude.Int
associateCertificateResponse_httpStatus = Lens.lens (\AssociateCertificateResponse' {httpStatus} -> httpStatus) (\s@AssociateCertificateResponse' {} a -> s {httpStatus = a} :: AssociateCertificateResponse)

instance Prelude.NFData AssociateCertificateResponse
