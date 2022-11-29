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
-- Module      : Amazonka.Transfer.DescribeAgreement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the agreement that\'s identified by the @AgreementId@.
module Amazonka.Transfer.DescribeAgreement
  ( -- * Creating a Request
    DescribeAgreement (..),
    newDescribeAgreement,

    -- * Request Lenses
    describeAgreement_agreementId,
    describeAgreement_serverId,

    -- * Destructuring the Response
    DescribeAgreementResponse (..),
    newDescribeAgreementResponse,

    -- * Response Lenses
    describeAgreementResponse_httpStatus,
    describeAgreementResponse_agreement,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeAgreement' smart constructor.
data DescribeAgreement = DescribeAgreement'
  { -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Text,
    -- | The server identifier that\'s associated with the agreement.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agreementId', 'describeAgreement_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
--
-- 'serverId', 'describeAgreement_serverId' - The server identifier that\'s associated with the agreement.
newDescribeAgreement ::
  -- | 'agreementId'
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  DescribeAgreement
newDescribeAgreement pAgreementId_ pServerId_ =
  DescribeAgreement'
    { agreementId = pAgreementId_,
      serverId = pServerId_
    }

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
describeAgreement_agreementId :: Lens.Lens' DescribeAgreement Prelude.Text
describeAgreement_agreementId = Lens.lens (\DescribeAgreement' {agreementId} -> agreementId) (\s@DescribeAgreement' {} a -> s {agreementId = a} :: DescribeAgreement)

-- | The server identifier that\'s associated with the agreement.
describeAgreement_serverId :: Lens.Lens' DescribeAgreement Prelude.Text
describeAgreement_serverId = Lens.lens (\DescribeAgreement' {serverId} -> serverId) (\s@DescribeAgreement' {} a -> s {serverId = a} :: DescribeAgreement)

instance Core.AWSRequest DescribeAgreement where
  type
    AWSResponse DescribeAgreement =
      DescribeAgreementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgreementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Agreement")
      )

instance Prelude.Hashable DescribeAgreement where
  hashWithSalt _salt DescribeAgreement' {..} =
    _salt `Prelude.hashWithSalt` agreementId
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData DescribeAgreement where
  rnf DescribeAgreement' {..} =
    Prelude.rnf agreementId
      `Prelude.seq` Prelude.rnf serverId

instance Core.ToHeaders DescribeAgreement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DescribeAgreement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAgreement where
  toJSON DescribeAgreement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AgreementId" Core..= agreementId),
            Prelude.Just ("ServerId" Core..= serverId)
          ]
      )

instance Core.ToPath DescribeAgreement where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAgreement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAgreementResponse' smart constructor.
data DescribeAgreementResponse = DescribeAgreementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The details for the specified agreement, returned as a
    -- @DescribedAgreement@ object.
    agreement :: DescribedAgreement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgreementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAgreementResponse_httpStatus' - The response's http status code.
--
-- 'agreement', 'describeAgreementResponse_agreement' - The details for the specified agreement, returned as a
-- @DescribedAgreement@ object.
newDescribeAgreementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'agreement'
  DescribedAgreement ->
  DescribeAgreementResponse
newDescribeAgreementResponse pHttpStatus_ pAgreement_ =
  DescribeAgreementResponse'
    { httpStatus =
        pHttpStatus_,
      agreement = pAgreement_
    }

-- | The response's http status code.
describeAgreementResponse_httpStatus :: Lens.Lens' DescribeAgreementResponse Prelude.Int
describeAgreementResponse_httpStatus = Lens.lens (\DescribeAgreementResponse' {httpStatus} -> httpStatus) (\s@DescribeAgreementResponse' {} a -> s {httpStatus = a} :: DescribeAgreementResponse)

-- | The details for the specified agreement, returned as a
-- @DescribedAgreement@ object.
describeAgreementResponse_agreement :: Lens.Lens' DescribeAgreementResponse DescribedAgreement
describeAgreementResponse_agreement = Lens.lens (\DescribeAgreementResponse' {agreement} -> agreement) (\s@DescribeAgreementResponse' {} a -> s {agreement = a} :: DescribeAgreementResponse)

instance Prelude.NFData DescribeAgreementResponse where
  rnf DescribeAgreementResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf agreement
