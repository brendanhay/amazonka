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
-- Module      : Amazonka.GuardDuty.Types.MemberFeaturesConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.MemberFeaturesConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureStatus
import Amazonka.GuardDuty.Types.MemberAdditionalConfigurationResult
import Amazonka.GuardDuty.Types.OrgFeature
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the features for the member account.
--
-- /See:/ 'newMemberFeaturesConfigurationResult' smart constructor.
data MemberFeaturesConfigurationResult = MemberFeaturesConfigurationResult'
  { -- | Indicates the additional configuration of the feature that is configured
    -- for the member account.
    additionalConfiguration :: Prelude.Maybe [MemberAdditionalConfigurationResult],
    -- | Indicates the name of the feature that is enabled for the detector.
    name :: Prelude.Maybe OrgFeature,
    -- | Indicates the status of the feature that is enabled for the detector.
    status :: Prelude.Maybe FeatureStatus,
    -- | The timestamp at which the feature object was updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberFeaturesConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'memberFeaturesConfigurationResult_additionalConfiguration' - Indicates the additional configuration of the feature that is configured
-- for the member account.
--
-- 'name', 'memberFeaturesConfigurationResult_name' - Indicates the name of the feature that is enabled for the detector.
--
-- 'status', 'memberFeaturesConfigurationResult_status' - Indicates the status of the feature that is enabled for the detector.
--
-- 'updatedAt', 'memberFeaturesConfigurationResult_updatedAt' - The timestamp at which the feature object was updated.
newMemberFeaturesConfigurationResult ::
  MemberFeaturesConfigurationResult
newMemberFeaturesConfigurationResult =
  MemberFeaturesConfigurationResult'
    { additionalConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Indicates the additional configuration of the feature that is configured
-- for the member account.
memberFeaturesConfigurationResult_additionalConfiguration :: Lens.Lens' MemberFeaturesConfigurationResult (Prelude.Maybe [MemberAdditionalConfigurationResult])
memberFeaturesConfigurationResult_additionalConfiguration = Lens.lens (\MemberFeaturesConfigurationResult' {additionalConfiguration} -> additionalConfiguration) (\s@MemberFeaturesConfigurationResult' {} a -> s {additionalConfiguration = a} :: MemberFeaturesConfigurationResult) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the name of the feature that is enabled for the detector.
memberFeaturesConfigurationResult_name :: Lens.Lens' MemberFeaturesConfigurationResult (Prelude.Maybe OrgFeature)
memberFeaturesConfigurationResult_name = Lens.lens (\MemberFeaturesConfigurationResult' {name} -> name) (\s@MemberFeaturesConfigurationResult' {} a -> s {name = a} :: MemberFeaturesConfigurationResult)

-- | Indicates the status of the feature that is enabled for the detector.
memberFeaturesConfigurationResult_status :: Lens.Lens' MemberFeaturesConfigurationResult (Prelude.Maybe FeatureStatus)
memberFeaturesConfigurationResult_status = Lens.lens (\MemberFeaturesConfigurationResult' {status} -> status) (\s@MemberFeaturesConfigurationResult' {} a -> s {status = a} :: MemberFeaturesConfigurationResult)

-- | The timestamp at which the feature object was updated.
memberFeaturesConfigurationResult_updatedAt :: Lens.Lens' MemberFeaturesConfigurationResult (Prelude.Maybe Prelude.UTCTime)
memberFeaturesConfigurationResult_updatedAt = Lens.lens (\MemberFeaturesConfigurationResult' {updatedAt} -> updatedAt) (\s@MemberFeaturesConfigurationResult' {} a -> s {updatedAt = a} :: MemberFeaturesConfigurationResult) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    MemberFeaturesConfigurationResult
  where
  parseJSON =
    Data.withObject
      "MemberFeaturesConfigurationResult"
      ( \x ->
          MemberFeaturesConfigurationResult'
            Prelude.<$> ( x
                            Data..:? "additionalConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance
  Prelude.Hashable
    MemberFeaturesConfigurationResult
  where
  hashWithSalt
    _salt
    MemberFeaturesConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` additionalConfiguration
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` updatedAt

instance
  Prelude.NFData
    MemberFeaturesConfigurationResult
  where
  rnf MemberFeaturesConfigurationResult' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
