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
-- Module      : Amazonka.GuardDuty.Types.MemberFeaturesConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.MemberFeaturesConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureStatus
import Amazonka.GuardDuty.Types.MemberAdditionalConfiguration
import Amazonka.GuardDuty.Types.OrgFeature
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the features for the member account.
--
-- /See:/ 'newMemberFeaturesConfiguration' smart constructor.
data MemberFeaturesConfiguration = MemberFeaturesConfiguration'
  { -- | Additional configuration of the feature for the member account.
    additionalConfiguration :: Prelude.Maybe [MemberAdditionalConfiguration],
    -- | The name of the feature.
    name :: Prelude.Maybe OrgFeature,
    -- | The status of the feature.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberFeaturesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'memberFeaturesConfiguration_additionalConfiguration' - Additional configuration of the feature for the member account.
--
-- 'name', 'memberFeaturesConfiguration_name' - The name of the feature.
--
-- 'status', 'memberFeaturesConfiguration_status' - The status of the feature.
newMemberFeaturesConfiguration ::
  MemberFeaturesConfiguration
newMemberFeaturesConfiguration =
  MemberFeaturesConfiguration'
    { additionalConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Additional configuration of the feature for the member account.
memberFeaturesConfiguration_additionalConfiguration :: Lens.Lens' MemberFeaturesConfiguration (Prelude.Maybe [MemberAdditionalConfiguration])
memberFeaturesConfiguration_additionalConfiguration = Lens.lens (\MemberFeaturesConfiguration' {additionalConfiguration} -> additionalConfiguration) (\s@MemberFeaturesConfiguration' {} a -> s {additionalConfiguration = a} :: MemberFeaturesConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature.
memberFeaturesConfiguration_name :: Lens.Lens' MemberFeaturesConfiguration (Prelude.Maybe OrgFeature)
memberFeaturesConfiguration_name = Lens.lens (\MemberFeaturesConfiguration' {name} -> name) (\s@MemberFeaturesConfiguration' {} a -> s {name = a} :: MemberFeaturesConfiguration)

-- | The status of the feature.
memberFeaturesConfiguration_status :: Lens.Lens' MemberFeaturesConfiguration (Prelude.Maybe FeatureStatus)
memberFeaturesConfiguration_status = Lens.lens (\MemberFeaturesConfiguration' {status} -> status) (\s@MemberFeaturesConfiguration' {} a -> s {status = a} :: MemberFeaturesConfiguration)

instance Prelude.Hashable MemberFeaturesConfiguration where
  hashWithSalt _salt MemberFeaturesConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` additionalConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData MemberFeaturesConfiguration where
  rnf MemberFeaturesConfiguration' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON MemberFeaturesConfiguration where
  toJSON MemberFeaturesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalConfiguration" Data..=)
              Prelude.<$> additionalConfiguration,
            ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
