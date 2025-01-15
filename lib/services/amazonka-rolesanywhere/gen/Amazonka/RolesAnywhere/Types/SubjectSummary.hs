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
-- Module      : Amazonka.RolesAnywhere.Types.SubjectSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.SubjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary representation of Subject resources returned in read
-- operations; primarily ListSubjects.
--
-- /See:/ 'newSubjectSummary' smart constructor.
data SubjectSummary = SubjectSummary'
  { -- | The ISO-8601 time stamp of when the certificate was first used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The enabled status of the Subject.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ISO-8601 time stamp of when the certificate was last used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    lastSeenAt :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the resource.
    subjectArn :: Prelude.Maybe Prelude.Text,
    -- | The id of the resource.
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 timestamp when the subject was last updated.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The x509 principal identifier of the authenticating certificate.
    x509Subject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubjectSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'subjectSummary_createdAt' - The ISO-8601 time stamp of when the certificate was first used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'enabled', 'subjectSummary_enabled' - The enabled status of the Subject.
--
-- 'lastSeenAt', 'subjectSummary_lastSeenAt' - The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'subjectArn', 'subjectSummary_subjectArn' - The ARN of the resource.
--
-- 'subjectId', 'subjectSummary_subjectId' - The id of the resource.
--
-- 'updatedAt', 'subjectSummary_updatedAt' - The ISO-8601 timestamp when the subject was last updated.
--
-- 'x509Subject', 'subjectSummary_x509Subject' - The x509 principal identifier of the authenticating certificate.
newSubjectSummary ::
  SubjectSummary
newSubjectSummary =
  SubjectSummary'
    { createdAt = Prelude.Nothing,
      enabled = Prelude.Nothing,
      lastSeenAt = Prelude.Nothing,
      subjectArn = Prelude.Nothing,
      subjectId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      x509Subject = Prelude.Nothing
    }

-- | The ISO-8601 time stamp of when the certificate was first used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
subjectSummary_createdAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_createdAt = Lens.lens (\SubjectSummary' {createdAt} -> createdAt) (\s@SubjectSummary' {} a -> s {createdAt = a} :: SubjectSummary) Prelude.. Lens.mapping Data._Time

-- | The enabled status of the Subject.
subjectSummary_enabled :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Bool)
subjectSummary_enabled = Lens.lens (\SubjectSummary' {enabled} -> enabled) (\s@SubjectSummary' {} a -> s {enabled = a} :: SubjectSummary)

-- | The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
subjectSummary_lastSeenAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_lastSeenAt = Lens.lens (\SubjectSummary' {lastSeenAt} -> lastSeenAt) (\s@SubjectSummary' {} a -> s {lastSeenAt = a} :: SubjectSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the resource.
subjectSummary_subjectArn :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_subjectArn = Lens.lens (\SubjectSummary' {subjectArn} -> subjectArn) (\s@SubjectSummary' {} a -> s {subjectArn = a} :: SubjectSummary)

-- | The id of the resource.
subjectSummary_subjectId :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_subjectId = Lens.lens (\SubjectSummary' {subjectId} -> subjectId) (\s@SubjectSummary' {} a -> s {subjectId = a} :: SubjectSummary)

-- | The ISO-8601 timestamp when the subject was last updated.
subjectSummary_updatedAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_updatedAt = Lens.lens (\SubjectSummary' {updatedAt} -> updatedAt) (\s@SubjectSummary' {} a -> s {updatedAt = a} :: SubjectSummary) Prelude.. Lens.mapping Data._Time

-- | The x509 principal identifier of the authenticating certificate.
subjectSummary_x509Subject :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_x509Subject = Lens.lens (\SubjectSummary' {x509Subject} -> x509Subject) (\s@SubjectSummary' {} a -> s {x509Subject = a} :: SubjectSummary)

instance Data.FromJSON SubjectSummary where
  parseJSON =
    Data.withObject
      "SubjectSummary"
      ( \x ->
          SubjectSummary'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "lastSeenAt")
            Prelude.<*> (x Data..:? "subjectArn")
            Prelude.<*> (x Data..:? "subjectId")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..:? "x509Subject")
      )

instance Prelude.Hashable SubjectSummary where
  hashWithSalt _salt SubjectSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastSeenAt
      `Prelude.hashWithSalt` subjectArn
      `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` x509Subject

instance Prelude.NFData SubjectSummary where
  rnf SubjectSummary' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf enabled `Prelude.seq`
        Prelude.rnf lastSeenAt `Prelude.seq`
          Prelude.rnf subjectArn `Prelude.seq`
            Prelude.rnf subjectId `Prelude.seq`
              Prelude.rnf updatedAt `Prelude.seq`
                Prelude.rnf x509Subject
