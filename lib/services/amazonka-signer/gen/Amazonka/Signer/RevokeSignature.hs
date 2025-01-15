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
-- Module      : Amazonka.Signer.RevokeSignature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the state of a signing job to REVOKED. This indicates that the
-- signature is no longer valid.
module Amazonka.Signer.RevokeSignature
  ( -- * Creating a Request
    RevokeSignature (..),
    newRevokeSignature,

    -- * Request Lenses
    revokeSignature_jobOwner,
    revokeSignature_reason,
    revokeSignature_jobId,

    -- * Destructuring the Response
    RevokeSignatureResponse (..),
    newRevokeSignatureResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newRevokeSignature' smart constructor.
data RevokeSignature = RevokeSignature'
  { -- | AWS account ID of the job owner.
    jobOwner :: Prelude.Maybe Prelude.Text,
    -- | The reason for revoking the signing job.
    reason :: Prelude.Text,
    -- | ID of the signing job to be revoked.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSignature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobOwner', 'revokeSignature_jobOwner' - AWS account ID of the job owner.
--
-- 'reason', 'revokeSignature_reason' - The reason for revoking the signing job.
--
-- 'jobId', 'revokeSignature_jobId' - ID of the signing job to be revoked.
newRevokeSignature ::
  -- | 'reason'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  RevokeSignature
newRevokeSignature pReason_ pJobId_ =
  RevokeSignature'
    { jobOwner = Prelude.Nothing,
      reason = pReason_,
      jobId = pJobId_
    }

-- | AWS account ID of the job owner.
revokeSignature_jobOwner :: Lens.Lens' RevokeSignature (Prelude.Maybe Prelude.Text)
revokeSignature_jobOwner = Lens.lens (\RevokeSignature' {jobOwner} -> jobOwner) (\s@RevokeSignature' {} a -> s {jobOwner = a} :: RevokeSignature)

-- | The reason for revoking the signing job.
revokeSignature_reason :: Lens.Lens' RevokeSignature Prelude.Text
revokeSignature_reason = Lens.lens (\RevokeSignature' {reason} -> reason) (\s@RevokeSignature' {} a -> s {reason = a} :: RevokeSignature)

-- | ID of the signing job to be revoked.
revokeSignature_jobId :: Lens.Lens' RevokeSignature Prelude.Text
revokeSignature_jobId = Lens.lens (\RevokeSignature' {jobId} -> jobId) (\s@RevokeSignature' {} a -> s {jobId = a} :: RevokeSignature)

instance Core.AWSRequest RevokeSignature where
  type
    AWSResponse RevokeSignature =
      RevokeSignatureResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull RevokeSignatureResponse'

instance Prelude.Hashable RevokeSignature where
  hashWithSalt _salt RevokeSignature' {..} =
    _salt
      `Prelude.hashWithSalt` jobOwner
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData RevokeSignature where
  rnf RevokeSignature' {..} =
    Prelude.rnf jobOwner `Prelude.seq`
      Prelude.rnf reason `Prelude.seq`
        Prelude.rnf jobId

instance Data.ToHeaders RevokeSignature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RevokeSignature where
  toJSON RevokeSignature' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobOwner" Data..=) Prelude.<$> jobOwner,
            Prelude.Just ("reason" Data..= reason)
          ]
      )

instance Data.ToPath RevokeSignature where
  toPath RevokeSignature' {..} =
    Prelude.mconcat
      ["/signing-jobs/", Data.toBS jobId, "/revoke"]

instance Data.ToQuery RevokeSignature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeSignatureResponse' smart constructor.
data RevokeSignatureResponse = RevokeSignatureResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSignatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRevokeSignatureResponse ::
  RevokeSignatureResponse
newRevokeSignatureResponse = RevokeSignatureResponse'

instance Prelude.NFData RevokeSignatureResponse where
  rnf _ = ()
