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
-- Module      : Amazonka.GuardDuty.Types.MemberAdditionalConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.MemberAdditionalConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureStatus
import Amazonka.GuardDuty.Types.OrgFeatureAdditionalConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Information about the additional configuration for the member account.
--
-- /See:/ 'newMemberAdditionalConfigurationResult' smart constructor.
data MemberAdditionalConfigurationResult = MemberAdditionalConfigurationResult'
  { -- | Indicates the name of the additional configuration that is set for the
    -- member account.
    name :: Prelude.Maybe OrgFeatureAdditionalConfiguration,
    -- | Indicates the status of the additional configuration that is set for the
    -- member account.
    status :: Prelude.Maybe FeatureStatus,
    -- | The timestamp at which the additional configuration was set for the
    -- member account. This is in UTC format.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAdditionalConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'memberAdditionalConfigurationResult_name' - Indicates the name of the additional configuration that is set for the
-- member account.
--
-- 'status', 'memberAdditionalConfigurationResult_status' - Indicates the status of the additional configuration that is set for the
-- member account.
--
-- 'updatedAt', 'memberAdditionalConfigurationResult_updatedAt' - The timestamp at which the additional configuration was set for the
-- member account. This is in UTC format.
newMemberAdditionalConfigurationResult ::
  MemberAdditionalConfigurationResult
newMemberAdditionalConfigurationResult =
  MemberAdditionalConfigurationResult'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Indicates the name of the additional configuration that is set for the
-- member account.
memberAdditionalConfigurationResult_name :: Lens.Lens' MemberAdditionalConfigurationResult (Prelude.Maybe OrgFeatureAdditionalConfiguration)
memberAdditionalConfigurationResult_name = Lens.lens (\MemberAdditionalConfigurationResult' {name} -> name) (\s@MemberAdditionalConfigurationResult' {} a -> s {name = a} :: MemberAdditionalConfigurationResult)

-- | Indicates the status of the additional configuration that is set for the
-- member account.
memberAdditionalConfigurationResult_status :: Lens.Lens' MemberAdditionalConfigurationResult (Prelude.Maybe FeatureStatus)
memberAdditionalConfigurationResult_status = Lens.lens (\MemberAdditionalConfigurationResult' {status} -> status) (\s@MemberAdditionalConfigurationResult' {} a -> s {status = a} :: MemberAdditionalConfigurationResult)

-- | The timestamp at which the additional configuration was set for the
-- member account. This is in UTC format.
memberAdditionalConfigurationResult_updatedAt :: Lens.Lens' MemberAdditionalConfigurationResult (Prelude.Maybe Prelude.UTCTime)
memberAdditionalConfigurationResult_updatedAt = Lens.lens (\MemberAdditionalConfigurationResult' {updatedAt} -> updatedAt) (\s@MemberAdditionalConfigurationResult' {} a -> s {updatedAt = a} :: MemberAdditionalConfigurationResult) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    MemberAdditionalConfigurationResult
  where
  parseJSON =
    Data.withObject
      "MemberAdditionalConfigurationResult"
      ( \x ->
          MemberAdditionalConfigurationResult'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance
  Prelude.Hashable
    MemberAdditionalConfigurationResult
  where
  hashWithSalt
    _salt
    MemberAdditionalConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` updatedAt

instance
  Prelude.NFData
    MemberAdditionalConfigurationResult
  where
  rnf MemberAdditionalConfigurationResult' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
