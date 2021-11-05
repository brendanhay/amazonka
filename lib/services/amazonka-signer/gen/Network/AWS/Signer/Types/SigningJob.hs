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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.SignedObject
import Amazonka.Signer.Types.SigningMaterial
import Amazonka.Signer.Types.SigningStatus
import Amazonka.Signer.Types.Source

-- | Contains information about a signing job.
--
-- /See:/ 'newSigningJob' smart constructor.
data SigningJob = SigningJob'
  { -- | The status of the signing job.
    status :: Prelude.Maybe SigningStatus,
    -- | The name of a signing platform.
    platformDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the signing job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the signing job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
    -- of the certificate used for the signing job.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | Indicates whether the signing job is revoked.
    isRevoked :: Prelude.Maybe Prelude.Bool,
    -- | The time when the signature of a signing job expires.
    signatureExpiresAt :: Prelude.Maybe Core.POSIX,
    -- | The version of the signing profile that created a signing job.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the signing profile that created a signing job.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | A @SignedObject@ structure that contains information about a signing
    -- job\'s signed code image.
    signedObject :: Prelude.Maybe SignedObject,
    -- | The unique identifier for a signing platform.
    platformId :: Prelude.Maybe Prelude.Text,
    -- | A @Source@ that contains information about a signing job\'s code image
    -- source.
    source :: Prelude.Maybe Source,
    -- | The AWS account ID of the job invoker.
    jobInvoker :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the job owner.
    jobOwner :: Prelude.Maybe Prelude.Text
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
-- 'status', 'signingJob_status' - The status of the signing job.
--
-- 'platformDisplayName', 'signingJob_platformDisplayName' - The name of a signing platform.
--
-- 'jobId', 'signingJob_jobId' - The ID of the signing job.
--
-- 'createdAt', 'signingJob_createdAt' - The date and time that the signing job was created.
--
-- 'signingMaterial', 'signingJob_signingMaterial' - A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
-- of the certificate used for the signing job.
--
-- 'isRevoked', 'signingJob_isRevoked' - Indicates whether the signing job is revoked.
--
-- 'signatureExpiresAt', 'signingJob_signatureExpiresAt' - The time when the signature of a signing job expires.
--
-- 'profileVersion', 'signingJob_profileVersion' - The version of the signing profile that created a signing job.
--
-- 'profileName', 'signingJob_profileName' - The name of the signing profile that created a signing job.
--
-- 'signedObject', 'signingJob_signedObject' - A @SignedObject@ structure that contains information about a signing
-- job\'s signed code image.
--
-- 'platformId', 'signingJob_platformId' - The unique identifier for a signing platform.
--
-- 'source', 'signingJob_source' - A @Source@ that contains information about a signing job\'s code image
-- source.
--
-- 'jobInvoker', 'signingJob_jobInvoker' - The AWS account ID of the job invoker.
--
-- 'jobOwner', 'signingJob_jobOwner' - The AWS account ID of the job owner.
newSigningJob ::
  SigningJob
newSigningJob =
  SigningJob'
    { status = Prelude.Nothing,
      platformDisplayName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      isRevoked = Prelude.Nothing,
      signatureExpiresAt = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      profileName = Prelude.Nothing,
      signedObject = Prelude.Nothing,
      platformId = Prelude.Nothing,
      source = Prelude.Nothing,
      jobInvoker = Prelude.Nothing,
      jobOwner = Prelude.Nothing
    }

-- | The status of the signing job.
signingJob_status :: Lens.Lens' SigningJob (Prelude.Maybe SigningStatus)
signingJob_status = Lens.lens (\SigningJob' {status} -> status) (\s@SigningJob' {} a -> s {status = a} :: SigningJob)

-- | The name of a signing platform.
signingJob_platformDisplayName :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_platformDisplayName = Lens.lens (\SigningJob' {platformDisplayName} -> platformDisplayName) (\s@SigningJob' {} a -> s {platformDisplayName = a} :: SigningJob)

-- | The ID of the signing job.
signingJob_jobId :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobId = Lens.lens (\SigningJob' {jobId} -> jobId) (\s@SigningJob' {} a -> s {jobId = a} :: SigningJob)

-- | The date and time that the signing job was created.
signingJob_createdAt :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.UTCTime)
signingJob_createdAt = Lens.lens (\SigningJob' {createdAt} -> createdAt) (\s@SigningJob' {} a -> s {createdAt = a} :: SigningJob) Prelude.. Lens.mapping Core._Time

-- | A @SigningMaterial@ object that contains the Amazon Resource Name (ARN)
-- of the certificate used for the signing job.
signingJob_signingMaterial :: Lens.Lens' SigningJob (Prelude.Maybe SigningMaterial)
signingJob_signingMaterial = Lens.lens (\SigningJob' {signingMaterial} -> signingMaterial) (\s@SigningJob' {} a -> s {signingMaterial = a} :: SigningJob)

-- | Indicates whether the signing job is revoked.
signingJob_isRevoked :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Bool)
signingJob_isRevoked = Lens.lens (\SigningJob' {isRevoked} -> isRevoked) (\s@SigningJob' {} a -> s {isRevoked = a} :: SigningJob)

-- | The time when the signature of a signing job expires.
signingJob_signatureExpiresAt :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.UTCTime)
signingJob_signatureExpiresAt = Lens.lens (\SigningJob' {signatureExpiresAt} -> signatureExpiresAt) (\s@SigningJob' {} a -> s {signatureExpiresAt = a} :: SigningJob) Prelude.. Lens.mapping Core._Time

-- | The version of the signing profile that created a signing job.
signingJob_profileVersion :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_profileVersion = Lens.lens (\SigningJob' {profileVersion} -> profileVersion) (\s@SigningJob' {} a -> s {profileVersion = a} :: SigningJob)

-- | The name of the signing profile that created a signing job.
signingJob_profileName :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_profileName = Lens.lens (\SigningJob' {profileName} -> profileName) (\s@SigningJob' {} a -> s {profileName = a} :: SigningJob)

-- | A @SignedObject@ structure that contains information about a signing
-- job\'s signed code image.
signingJob_signedObject :: Lens.Lens' SigningJob (Prelude.Maybe SignedObject)
signingJob_signedObject = Lens.lens (\SigningJob' {signedObject} -> signedObject) (\s@SigningJob' {} a -> s {signedObject = a} :: SigningJob)

-- | The unique identifier for a signing platform.
signingJob_platformId :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_platformId = Lens.lens (\SigningJob' {platformId} -> platformId) (\s@SigningJob' {} a -> s {platformId = a} :: SigningJob)

-- | A @Source@ that contains information about a signing job\'s code image
-- source.
signingJob_source :: Lens.Lens' SigningJob (Prelude.Maybe Source)
signingJob_source = Lens.lens (\SigningJob' {source} -> source) (\s@SigningJob' {} a -> s {source = a} :: SigningJob)

-- | The AWS account ID of the job invoker.
signingJob_jobInvoker :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobInvoker = Lens.lens (\SigningJob' {jobInvoker} -> jobInvoker) (\s@SigningJob' {} a -> s {jobInvoker = a} :: SigningJob)

-- | The AWS account ID of the job owner.
signingJob_jobOwner :: Lens.Lens' SigningJob (Prelude.Maybe Prelude.Text)
signingJob_jobOwner = Lens.lens (\SigningJob' {jobOwner} -> jobOwner) (\s@SigningJob' {} a -> s {jobOwner = a} :: SigningJob)

instance Core.FromJSON SigningJob where
  parseJSON =
    Core.withObject
      "SigningJob"
      ( \x ->
          SigningJob'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "platformDisplayName")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "signingMaterial")
            Prelude.<*> (x Core..:? "isRevoked")
            Prelude.<*> (x Core..:? "signatureExpiresAt")
            Prelude.<*> (x Core..:? "profileVersion")
            Prelude.<*> (x Core..:? "profileName")
            Prelude.<*> (x Core..:? "signedObject")
            Prelude.<*> (x Core..:? "platformId")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "jobInvoker")
            Prelude.<*> (x Core..:? "jobOwner")
      )

instance Prelude.Hashable SigningJob

instance Prelude.NFData SigningJob
