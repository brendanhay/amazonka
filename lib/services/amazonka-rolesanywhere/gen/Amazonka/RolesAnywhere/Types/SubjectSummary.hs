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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.SubjectSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary representation of Subject resources returned in read
-- operations; primarily ListSubjects.
--
-- /See:/ 'newSubjectSummary' smart constructor.
data SubjectSummary = SubjectSummary'
  { -- | The id of the resource.
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The enabled status of the Subject.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the resource.
    subjectArn :: Prelude.Maybe Prelude.Text,
    -- | The x509 principal identifier of the authenticating certificate.
    x509Subject :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 time stamp of when the certificate was first used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ISO-8601 timestamp when the subject was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The ISO-8601 time stamp of when the certificate was last used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    lastSeenAt :: Prelude.Maybe Core.POSIX
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
-- 'subjectId', 'subjectSummary_subjectId' - The id of the resource.
--
-- 'enabled', 'subjectSummary_enabled' - The enabled status of the Subject.
--
-- 'subjectArn', 'subjectSummary_subjectArn' - The ARN of the resource.
--
-- 'x509Subject', 'subjectSummary_x509Subject' - The x509 principal identifier of the authenticating certificate.
--
-- 'createdAt', 'subjectSummary_createdAt' - The ISO-8601 time stamp of when the certificate was first used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'updatedAt', 'subjectSummary_updatedAt' - The ISO-8601 timestamp when the subject was last updated.
--
-- 'lastSeenAt', 'subjectSummary_lastSeenAt' - The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
newSubjectSummary ::
  SubjectSummary
newSubjectSummary =
  SubjectSummary'
    { subjectId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      subjectArn = Prelude.Nothing,
      x509Subject = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      lastSeenAt = Prelude.Nothing
    }

-- | The id of the resource.
subjectSummary_subjectId :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_subjectId = Lens.lens (\SubjectSummary' {subjectId} -> subjectId) (\s@SubjectSummary' {} a -> s {subjectId = a} :: SubjectSummary)

-- | The enabled status of the Subject.
subjectSummary_enabled :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Bool)
subjectSummary_enabled = Lens.lens (\SubjectSummary' {enabled} -> enabled) (\s@SubjectSummary' {} a -> s {enabled = a} :: SubjectSummary)

-- | The ARN of the resource.
subjectSummary_subjectArn :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_subjectArn = Lens.lens (\SubjectSummary' {subjectArn} -> subjectArn) (\s@SubjectSummary' {} a -> s {subjectArn = a} :: SubjectSummary)

-- | The x509 principal identifier of the authenticating certificate.
subjectSummary_x509Subject :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.Text)
subjectSummary_x509Subject = Lens.lens (\SubjectSummary' {x509Subject} -> x509Subject) (\s@SubjectSummary' {} a -> s {x509Subject = a} :: SubjectSummary)

-- | The ISO-8601 time stamp of when the certificate was first used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
subjectSummary_createdAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_createdAt = Lens.lens (\SubjectSummary' {createdAt} -> createdAt) (\s@SubjectSummary' {} a -> s {createdAt = a} :: SubjectSummary) Prelude.. Lens.mapping Core._Time

-- | The ISO-8601 timestamp when the subject was last updated.
subjectSummary_updatedAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_updatedAt = Lens.lens (\SubjectSummary' {updatedAt} -> updatedAt) (\s@SubjectSummary' {} a -> s {updatedAt = a} :: SubjectSummary) Prelude.. Lens.mapping Core._Time

-- | The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
subjectSummary_lastSeenAt :: Lens.Lens' SubjectSummary (Prelude.Maybe Prelude.UTCTime)
subjectSummary_lastSeenAt = Lens.lens (\SubjectSummary' {lastSeenAt} -> lastSeenAt) (\s@SubjectSummary' {} a -> s {lastSeenAt = a} :: SubjectSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SubjectSummary where
  parseJSON =
    Core.withObject
      "SubjectSummary"
      ( \x ->
          SubjectSummary'
            Prelude.<$> (x Core..:? "subjectId")
            Prelude.<*> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "subjectArn")
            Prelude.<*> (x Core..:? "x509Subject")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "lastSeenAt")
      )

instance Prelude.Hashable SubjectSummary where
  hashWithSalt _salt SubjectSummary' {..} =
    _salt `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` subjectArn
      `Prelude.hashWithSalt` x509Subject
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` lastSeenAt

instance Prelude.NFData SubjectSummary where
  rnf SubjectSummary' {..} =
    Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf subjectArn
      `Prelude.seq` Prelude.rnf x509Subject
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf lastSeenAt
