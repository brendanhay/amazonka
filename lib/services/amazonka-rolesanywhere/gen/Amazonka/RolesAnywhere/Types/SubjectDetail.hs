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
-- Module      : Amazonka.RolesAnywhere.Types.SubjectDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.SubjectDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.CredentialSummary
import Amazonka.RolesAnywhere.Types.InstanceProperty

-- | The state of the subject after a read or write operation.
--
-- /See:/ 'newSubjectDetail' smart constructor.
data SubjectDetail = SubjectDetail'
  { -- | The id of the resource
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The enabled status of the subject.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the resource.
    subjectArn :: Prelude.Maybe Prelude.Text,
    -- | The temporary session credentials vended at the last authenticating call
    -- with this Subject.
    credentials :: Prelude.Maybe [CredentialSummary],
    -- | The x509 principal identifier of the authenticating certificate.
    x509Subject :: Prelude.Maybe Prelude.Text,
    -- | The specified instance properties associated with the request.
    instanceProperties :: Prelude.Maybe [InstanceProperty],
    -- | The ISO-8601 timestamp when the subject was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ISO-8601 timestamp when the subject was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The ISO-8601 timestamp of the last time this Subject requested temporary
    -- session credentials.
    lastSeenAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubjectDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectId', 'subjectDetail_subjectId' - The id of the resource
--
-- 'enabled', 'subjectDetail_enabled' - The enabled status of the subject.
--
-- 'subjectArn', 'subjectDetail_subjectArn' - The ARN of the resource.
--
-- 'credentials', 'subjectDetail_credentials' - The temporary session credentials vended at the last authenticating call
-- with this Subject.
--
-- 'x509Subject', 'subjectDetail_x509Subject' - The x509 principal identifier of the authenticating certificate.
--
-- 'instanceProperties', 'subjectDetail_instanceProperties' - The specified instance properties associated with the request.
--
-- 'createdAt', 'subjectDetail_createdAt' - The ISO-8601 timestamp when the subject was created.
--
-- 'updatedAt', 'subjectDetail_updatedAt' - The ISO-8601 timestamp when the subject was last updated.
--
-- 'lastSeenAt', 'subjectDetail_lastSeenAt' - The ISO-8601 timestamp of the last time this Subject requested temporary
-- session credentials.
newSubjectDetail ::
  SubjectDetail
newSubjectDetail =
  SubjectDetail'
    { subjectId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      subjectArn = Prelude.Nothing,
      credentials = Prelude.Nothing,
      x509Subject = Prelude.Nothing,
      instanceProperties = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      lastSeenAt = Prelude.Nothing
    }

-- | The id of the resource
subjectDetail_subjectId :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.Text)
subjectDetail_subjectId = Lens.lens (\SubjectDetail' {subjectId} -> subjectId) (\s@SubjectDetail' {} a -> s {subjectId = a} :: SubjectDetail)

-- | The enabled status of the subject.
subjectDetail_enabled :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.Bool)
subjectDetail_enabled = Lens.lens (\SubjectDetail' {enabled} -> enabled) (\s@SubjectDetail' {} a -> s {enabled = a} :: SubjectDetail)

-- | The ARN of the resource.
subjectDetail_subjectArn :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.Text)
subjectDetail_subjectArn = Lens.lens (\SubjectDetail' {subjectArn} -> subjectArn) (\s@SubjectDetail' {} a -> s {subjectArn = a} :: SubjectDetail)

-- | The temporary session credentials vended at the last authenticating call
-- with this Subject.
subjectDetail_credentials :: Lens.Lens' SubjectDetail (Prelude.Maybe [CredentialSummary])
subjectDetail_credentials = Lens.lens (\SubjectDetail' {credentials} -> credentials) (\s@SubjectDetail' {} a -> s {credentials = a} :: SubjectDetail) Prelude.. Lens.mapping Lens.coerced

-- | The x509 principal identifier of the authenticating certificate.
subjectDetail_x509Subject :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.Text)
subjectDetail_x509Subject = Lens.lens (\SubjectDetail' {x509Subject} -> x509Subject) (\s@SubjectDetail' {} a -> s {x509Subject = a} :: SubjectDetail)

-- | The specified instance properties associated with the request.
subjectDetail_instanceProperties :: Lens.Lens' SubjectDetail (Prelude.Maybe [InstanceProperty])
subjectDetail_instanceProperties = Lens.lens (\SubjectDetail' {instanceProperties} -> instanceProperties) (\s@SubjectDetail' {} a -> s {instanceProperties = a} :: SubjectDetail) Prelude.. Lens.mapping Lens.coerced

-- | The ISO-8601 timestamp when the subject was created.
subjectDetail_createdAt :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.UTCTime)
subjectDetail_createdAt = Lens.lens (\SubjectDetail' {createdAt} -> createdAt) (\s@SubjectDetail' {} a -> s {createdAt = a} :: SubjectDetail) Prelude.. Lens.mapping Data._Time

-- | The ISO-8601 timestamp when the subject was last updated.
subjectDetail_updatedAt :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.UTCTime)
subjectDetail_updatedAt = Lens.lens (\SubjectDetail' {updatedAt} -> updatedAt) (\s@SubjectDetail' {} a -> s {updatedAt = a} :: SubjectDetail) Prelude.. Lens.mapping Data._Time

-- | The ISO-8601 timestamp of the last time this Subject requested temporary
-- session credentials.
subjectDetail_lastSeenAt :: Lens.Lens' SubjectDetail (Prelude.Maybe Prelude.UTCTime)
subjectDetail_lastSeenAt = Lens.lens (\SubjectDetail' {lastSeenAt} -> lastSeenAt) (\s@SubjectDetail' {} a -> s {lastSeenAt = a} :: SubjectDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SubjectDetail where
  parseJSON =
    Data.withObject
      "SubjectDetail"
      ( \x ->
          SubjectDetail'
            Prelude.<$> (x Data..:? "subjectId")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "subjectArn")
            Prelude.<*> (x Data..:? "credentials" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "x509Subject")
            Prelude.<*> ( x Data..:? "instanceProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..:? "lastSeenAt")
      )

instance Prelude.Hashable SubjectDetail where
  hashWithSalt _salt SubjectDetail' {..} =
    _salt `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` subjectArn
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` x509Subject
      `Prelude.hashWithSalt` instanceProperties
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` lastSeenAt

instance Prelude.NFData SubjectDetail where
  rnf SubjectDetail' {..} =
    Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf subjectArn
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf x509Subject
      `Prelude.seq` Prelude.rnf instanceProperties
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf lastSeenAt
