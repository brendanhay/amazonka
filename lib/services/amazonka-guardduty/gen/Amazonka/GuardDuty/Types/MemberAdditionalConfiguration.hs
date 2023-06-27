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
-- Module      : Amazonka.GuardDuty.Types.MemberAdditionalConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.MemberAdditionalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureStatus
import Amazonka.GuardDuty.Types.OrgFeatureAdditionalConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Information about the additional configuration for the member account.
--
-- /See:/ 'newMemberAdditionalConfiguration' smart constructor.
data MemberAdditionalConfiguration = MemberAdditionalConfiguration'
  { -- | Name of the additional configuration.
    name :: Prelude.Maybe OrgFeatureAdditionalConfiguration,
    -- | Status of the additional configuration.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAdditionalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'memberAdditionalConfiguration_name' - Name of the additional configuration.
--
-- 'status', 'memberAdditionalConfiguration_status' - Status of the additional configuration.
newMemberAdditionalConfiguration ::
  MemberAdditionalConfiguration
newMemberAdditionalConfiguration =
  MemberAdditionalConfiguration'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Name of the additional configuration.
memberAdditionalConfiguration_name :: Lens.Lens' MemberAdditionalConfiguration (Prelude.Maybe OrgFeatureAdditionalConfiguration)
memberAdditionalConfiguration_name = Lens.lens (\MemberAdditionalConfiguration' {name} -> name) (\s@MemberAdditionalConfiguration' {} a -> s {name = a} :: MemberAdditionalConfiguration)

-- | Status of the additional configuration.
memberAdditionalConfiguration_status :: Lens.Lens' MemberAdditionalConfiguration (Prelude.Maybe FeatureStatus)
memberAdditionalConfiguration_status = Lens.lens (\MemberAdditionalConfiguration' {status} -> status) (\s@MemberAdditionalConfiguration' {} a -> s {status = a} :: MemberAdditionalConfiguration)

instance
  Prelude.Hashable
    MemberAdditionalConfiguration
  where
  hashWithSalt _salt MemberAdditionalConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData MemberAdditionalConfiguration where
  rnf MemberAdditionalConfiguration' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status

instance Data.ToJSON MemberAdditionalConfiguration where
  toJSON MemberAdditionalConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
