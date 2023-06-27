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
-- Module      : Amazonka.Signer.GetRevocationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the revocation status of one or more of the signing profile,
-- signing job, and signing certificate.
module Amazonka.Signer.GetRevocationStatus
  ( -- * Creating a Request
    GetRevocationStatus (..),
    newGetRevocationStatus,

    -- * Request Lenses
    getRevocationStatus_signatureTimestamp,
    getRevocationStatus_platformId,
    getRevocationStatus_profileVersionArn,
    getRevocationStatus_jobArn,
    getRevocationStatus_certificateHashes,

    -- * Destructuring the Response
    GetRevocationStatusResponse (..),
    newGetRevocationStatusResponse,

    -- * Response Lenses
    getRevocationStatusResponse_revokedEntities,
    getRevocationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newGetRevocationStatus' smart constructor.
data GetRevocationStatus = GetRevocationStatus'
  { -- | The timestamp of the signature that validates the profile or job.
    signatureTimestamp :: Data.POSIX,
    -- | The ID of a signing platform.
    platformId :: Prelude.Text,
    -- | The version of a signing profile.
    profileVersionArn :: Prelude.Text,
    -- | The ARN of a signing job.
    jobArn :: Prelude.Text,
    -- | A list of composite signed hashes that identify certificates.
    --
    -- A certificate identifier consists of a subject certificate TBS hash
    -- (signed by the parent CA) combined with a parent CA TBS hash (signed by
    -- the parent CA’s CA). Root certificates are defined as their own CA.
    certificateHashes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevocationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signatureTimestamp', 'getRevocationStatus_signatureTimestamp' - The timestamp of the signature that validates the profile or job.
--
-- 'platformId', 'getRevocationStatus_platformId' - The ID of a signing platform.
--
-- 'profileVersionArn', 'getRevocationStatus_profileVersionArn' - The version of a signing profile.
--
-- 'jobArn', 'getRevocationStatus_jobArn' - The ARN of a signing job.
--
-- 'certificateHashes', 'getRevocationStatus_certificateHashes' - A list of composite signed hashes that identify certificates.
--
-- A certificate identifier consists of a subject certificate TBS hash
-- (signed by the parent CA) combined with a parent CA TBS hash (signed by
-- the parent CA’s CA). Root certificates are defined as their own CA.
newGetRevocationStatus ::
  -- | 'signatureTimestamp'
  Prelude.UTCTime ->
  -- | 'platformId'
  Prelude.Text ->
  -- | 'profileVersionArn'
  Prelude.Text ->
  -- | 'jobArn'
  Prelude.Text ->
  GetRevocationStatus
newGetRevocationStatus
  pSignatureTimestamp_
  pPlatformId_
  pProfileVersionArn_
  pJobArn_ =
    GetRevocationStatus'
      { signatureTimestamp =
          Data._Time Lens.# pSignatureTimestamp_,
        platformId = pPlatformId_,
        profileVersionArn = pProfileVersionArn_,
        jobArn = pJobArn_,
        certificateHashes = Prelude.mempty
      }

-- | The timestamp of the signature that validates the profile or job.
getRevocationStatus_signatureTimestamp :: Lens.Lens' GetRevocationStatus Prelude.UTCTime
getRevocationStatus_signatureTimestamp = Lens.lens (\GetRevocationStatus' {signatureTimestamp} -> signatureTimestamp) (\s@GetRevocationStatus' {} a -> s {signatureTimestamp = a} :: GetRevocationStatus) Prelude.. Data._Time

-- | The ID of a signing platform.
getRevocationStatus_platformId :: Lens.Lens' GetRevocationStatus Prelude.Text
getRevocationStatus_platformId = Lens.lens (\GetRevocationStatus' {platformId} -> platformId) (\s@GetRevocationStatus' {} a -> s {platformId = a} :: GetRevocationStatus)

-- | The version of a signing profile.
getRevocationStatus_profileVersionArn :: Lens.Lens' GetRevocationStatus Prelude.Text
getRevocationStatus_profileVersionArn = Lens.lens (\GetRevocationStatus' {profileVersionArn} -> profileVersionArn) (\s@GetRevocationStatus' {} a -> s {profileVersionArn = a} :: GetRevocationStatus)

-- | The ARN of a signing job.
getRevocationStatus_jobArn :: Lens.Lens' GetRevocationStatus Prelude.Text
getRevocationStatus_jobArn = Lens.lens (\GetRevocationStatus' {jobArn} -> jobArn) (\s@GetRevocationStatus' {} a -> s {jobArn = a} :: GetRevocationStatus)

-- | A list of composite signed hashes that identify certificates.
--
-- A certificate identifier consists of a subject certificate TBS hash
-- (signed by the parent CA) combined with a parent CA TBS hash (signed by
-- the parent CA’s CA). Root certificates are defined as their own CA.
getRevocationStatus_certificateHashes :: Lens.Lens' GetRevocationStatus [Prelude.Text]
getRevocationStatus_certificateHashes = Lens.lens (\GetRevocationStatus' {certificateHashes} -> certificateHashes) (\s@GetRevocationStatus' {} a -> s {certificateHashes = a} :: GetRevocationStatus) Prelude.. Lens.coerced

instance Core.AWSRequest GetRevocationStatus where
  type
    AWSResponse GetRevocationStatus =
      GetRevocationStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRevocationStatusResponse'
            Prelude.<$> ( x
                            Data..?> "revokedEntities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRevocationStatus where
  hashWithSalt _salt GetRevocationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` signatureTimestamp
      `Prelude.hashWithSalt` platformId
      `Prelude.hashWithSalt` profileVersionArn
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` certificateHashes

instance Prelude.NFData GetRevocationStatus where
  rnf GetRevocationStatus' {..} =
    Prelude.rnf signatureTimestamp
      `Prelude.seq` Prelude.rnf platformId
      `Prelude.seq` Prelude.rnf profileVersionArn
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf certificateHashes

instance Data.ToHeaders GetRevocationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRevocationStatus where
  toPath = Prelude.const "/revocations"

instance Data.ToQuery GetRevocationStatus where
  toQuery GetRevocationStatus' {..} =
    Prelude.mconcat
      [ "signatureTimestamp" Data.=: signatureTimestamp,
        "platformId" Data.=: platformId,
        "profileVersionArn" Data.=: profileVersionArn,
        "jobArn" Data.=: jobArn,
        "certificateHashes"
          Data.=: Data.toQueryList "member" certificateHashes
      ]

-- | /See:/ 'newGetRevocationStatusResponse' smart constructor.
data GetRevocationStatusResponse = GetRevocationStatusResponse'
  { -- | A list of revoked entities (including one or more of the signing profile
    -- ARN, signing job ID, and certificate hash) supplied as input to the API.
    revokedEntities :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevocationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokedEntities', 'getRevocationStatusResponse_revokedEntities' - A list of revoked entities (including one or more of the signing profile
-- ARN, signing job ID, and certificate hash) supplied as input to the API.
--
-- 'httpStatus', 'getRevocationStatusResponse_httpStatus' - The response's http status code.
newGetRevocationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRevocationStatusResponse
newGetRevocationStatusResponse pHttpStatus_ =
  GetRevocationStatusResponse'
    { revokedEntities =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of revoked entities (including one or more of the signing profile
-- ARN, signing job ID, and certificate hash) supplied as input to the API.
getRevocationStatusResponse_revokedEntities :: Lens.Lens' GetRevocationStatusResponse (Prelude.Maybe [Prelude.Text])
getRevocationStatusResponse_revokedEntities = Lens.lens (\GetRevocationStatusResponse' {revokedEntities} -> revokedEntities) (\s@GetRevocationStatusResponse' {} a -> s {revokedEntities = a} :: GetRevocationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRevocationStatusResponse_httpStatus :: Lens.Lens' GetRevocationStatusResponse Prelude.Int
getRevocationStatusResponse_httpStatus = Lens.lens (\GetRevocationStatusResponse' {httpStatus} -> httpStatus) (\s@GetRevocationStatusResponse' {} a -> s {httpStatus = a} :: GetRevocationStatusResponse)

instance Prelude.NFData GetRevocationStatusResponse where
  rnf GetRevocationStatusResponse' {..} =
    Prelude.rnf revokedEntities
      `Prelude.seq` Prelude.rnf httpStatus
