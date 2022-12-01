{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.Types.SigningJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.SignedObject
import Amazonka.Signer.Types.SigningMaterial
import Amazonka.Signer.Types.SigningStatus
import Amazonka.Signer.Types.Source

-- | Contains information about a signing job.
--
-- /See:/ 'newSigningJob' smart constructor.
data SigningJob = SigningJob'
  { -- | The AWS account ID of the job owner.
    jobOwner :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the job invoker.
    jobInvoker :: Prelude.Maybe Prelude.Text,
    -- | The name of the signing profile that created a signing job.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | A @SignedObject@ structure that contains information about a signing
    -- job\'s signed code image.
    signedObject :: Prelude.Maybe SignedObject,
    -- | The name of a signing platform.
    platformDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the signing job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The status of the signing job.
    status :: Prelude.Maybe SigningStatus,
    -- | The version of the signing profile that created a signing job.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
    -- of the certificate used for the signing job.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | A @Source@ that contains information about a signing job\'s code image
    -- source.
    source :: Prelude.Maybe Source,
    -- | Indicates whether the signing job is revoked.
    isRevoked :: Prelude.Maybe Prelude.Bool,
    -- | The time when the signature of a signing job expires.
    signatureExpiresAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the signing job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for a signing platform.
    platformId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobOwner', 'signingJob_jobOwner' - The AWS account ID of the job owner.
--
-- 'jobInvoker', 'signingJob_jobInvoker' - The AWS account ID of the job invoker.
--
-- 'profileName', 'signingJob_profileName' - The name of the signing profile that created a signing job.
--
-- 'signedObject', 'signingJob_signedObject' - A @SignedObject@ structure that contains information about a signing
-- job\'s signed code image.
--
-- 'platformDisplayName', 'signingJob_platformDisplayName' - The name of a signing platform.
--
-- 'jobId', 'signingJob_jobId' - The ID of the signing job.
--
-- 'status', 'signingJob_status' - The status of the signing job.
--
-- 'profileVersion', 'signingJob_profileVersion' - The version of the signing profile that created a signing job.
--
-- 'signingMaterial', 'signingJob_signingMaterial' - A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
-- of the certificate used for the signing job.
--
-- 'source', 'signingJob_source' - A @Source@ that contains information about a signing job\'s code image
-- source.
--
-- 'isRevoked', 'signingJob_isRevoked' - Indicates whether the signing job is revoked.
--
-- 'signatureExpiresAt', 'signingJob_signatureExpiresAt' - The time when the signature of a signing job expires.
--
-- 'createdAt', 'signingJob_createdAt' - The date and time that the signing job was created.
--
-- 'platformId', 'signingJob_platformId' - The unique identifier for a signing platform.
newSigningJob ::
  SigningJob
newSigningJob =
  SigningJob'
    { jobOwner = Prelude.Nothing,
      jobInvoker = Prelude.Nothing,
      profileName = Prelude.Nothing,
      signedObject = Prelude.Nothing,
      platformDisplayName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      source = Prelude.Nothing,
      isRevoked = Prelude.Nothing,
      signatureExpiresAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      platformId = Prelude.Nothing
    }

-- | The AWS account ID of the job owner.
signingJob_jobOwner :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobOwner = Lens.lens (\SigningJob' {jobOwner} -> jobOwner) (\s@SigningJob' {} a -> s {jobOwner = a} :: SigningJob)

-- | The AWS account ID of the job invoker.
signingJob_jobInvoker :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobInvoker = Lens.lens (\SigningJob' {jobInvoker} -> jobInvoker) (\s@SigningJob' {} a -> s {jobInvoker = a} :: SigningJob)

-- | The name of the signing profile that created a signing job.
signingJob_profileName :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_profileName = Lens.lens (\SigningJob' {profileName} -> profileName) (\s@SigningJob' {} a -> s {profileName = a} :: SigningJob)

-- | A @SignedObject@ structure that contains information about a signing
-- job\'s signed code image.
signingJob_signedObject :: Lens.Lens' SigningJob (Prelude.Maybe SignedObject)
signingJob_signedObject = Lens.lens (\SigningJob' {signedObject} -> signedObject) (\s@SigningJob' {} a -> s {signedObject = a} :: SigningJob)

-- | The name of a signing platform.
signingJob_platformDisplayName :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_platformDisplayName = Lens.lens (\SigningJob' {platformDisplayName} -> platformDisplayName) (\s@SigningJob' {} a -> s {platformDisplayName = a} :: SigningJob)

-- | The ID of the signing job.
signingJob_jobId :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobId = Lens.lens (\SigningJob' {jobId} -> jobId) (\s@SigningJob' {} a -> s {jobId = a} :: SigningJob)

-- | The status of the signing job.
signingJob_status :: Lens.Lens' SigningJob (Prelude.Maybe SigningStatus)
signingJob_status = Lens.lens (\SigningJob' {status} -> status) (\s@SigningJob' {} a -> s {status = a} :: SigningJob)

-- | The version of the signing profile that created a signing job.
signingJob_profileVersion :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_profileVersion = Lens.lens (\SigningJob' {profileVersion} -> profileVersion) (\s@SigningJob' {} a -> s {profileVersion = a} :: SigningJob)

-- | A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
-- of the certificate used for the signing job.
signingJob_signingMaterial :: Lens.Lens' SigningJob (Prelude.Maybe SigningMaterial)
signingJob_signingMaterial = Lens.lens (\SigningJob' {signingMaterial} -> signingMaterial) (\s@SigningJob' {} a -> s {signingMaterial = a} :: SigningJob)

-- | A @Source@ that contains information about a signing job\'s code image
-- source.
signingJob_source :: Lens.Lens' SigningJob (Prelude.Maybe Source)
signingJob_source = Lens.lens (\SigningJob' {source} -> source) (\s@SigningJob' {} a -> s {source = a} :: SigningJob)

-- | Indicates whether the signing job is revoked.
signingJob_isRevoked :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Bool)
signingJob_isRevoked = Lens.lens (\SigningJob' {isRevoked} -> isRevoked) (\s@SigningJob' {} a -> s {isRevoked = a} :: SigningJob)

-- | The time when the signature of a signing job expires.
signingJob_signatureExpiresAt :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.UTCTime)
signingJob_signatureExpiresAt = Lens.lens (\SigningJob' {signatureExpiresAt} -> signatureExpiresAt) (\s@SigningJob' {} a -> s {signatureExpiresAt = a} :: SigningJob) Prelude.. Lens.mapping Core._Time

-- | The date and time that the signing job was created.
signingJob_createdAt :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.UTCTime)
signingJob_createdAt = Lens.lens (\SigningJob' {createdAt} -> createdAt) (\s@SigningJob' {} a -> s {createdAt = a} :: SigningJob) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for a signing platform.
signingJob_platformId :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_platformId = Lens.lens (\SigningJob' {platformId} -> platformId) (\s@SigningJob' {} a -> s {platformId = a} :: SigningJob)

instance Core.FromJSON SigningJob where
  parseJSON =
    Core.withObject
      "SigningJob"
      ( \x ->
          SigningJob'
            Prelude.<$> (x Core..:? "jobOwner")
            Prelude.<*> (x Core..:? "jobInvoker")
            Prelude.<*> (x Core..:? "profileName")
            Prelude.<*> (x Core..:? "signedObject")
            Prelude.<*> (x Core..:? "platformDisplayName")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "profileVersion")
            Prelude.<*> (x Core..:? "signingMaterial")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "isRevoked")
            Prelude.<*> (x Core..:? "signatureExpiresAt")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "platformId")
      )

instance Prelude.Hashable SigningJob where
  hashWithSalt _salt SigningJob' {..} =
    _salt `Prelude.hashWithSalt` jobOwner
      `Prelude.hashWithSalt` jobInvoker
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` signedObject
      `Prelude.hashWithSalt` platformDisplayName
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` signingMaterial
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` isRevoked
      `Prelude.hashWithSalt` signatureExpiresAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` platformId

instance Prelude.NFData SigningJob where
  rnf SigningJob' {..} =
    Prelude.rnf jobOwner
      `Prelude.seq` Prelude.rnf jobInvoker
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf signedObject
      `Prelude.seq` Prelude.rnf platformDisplayName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf signingMaterial
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf isRevoked
      `Prelude.seq` Prelude.rnf signatureExpiresAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf platformId
