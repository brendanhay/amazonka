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
-- Module      : Amazonka.DataExchange.Types.LakeFormationDataPermissionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.LakeFormationDataPermissionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFTagPolicyDetails
import qualified Amazonka.Prelude as Prelude

-- | Details about the AWS Lake Formation data permission.
--
-- /See:/ 'newLakeFormationDataPermissionDetails' smart constructor.
data LakeFormationDataPermissionDetails = LakeFormationDataPermissionDetails'
  { -- | Details about the LF-tag policy.
    lFTagPolicy :: Prelude.Maybe LFTagPolicyDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LakeFormationDataPermissionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lFTagPolicy', 'lakeFormationDataPermissionDetails_lFTagPolicy' - Details about the LF-tag policy.
newLakeFormationDataPermissionDetails ::
  LakeFormationDataPermissionDetails
newLakeFormationDataPermissionDetails =
  LakeFormationDataPermissionDetails'
    { lFTagPolicy =
        Prelude.Nothing
    }

-- | Details about the LF-tag policy.
lakeFormationDataPermissionDetails_lFTagPolicy :: Lens.Lens' LakeFormationDataPermissionDetails (Prelude.Maybe LFTagPolicyDetails)
lakeFormationDataPermissionDetails_lFTagPolicy = Lens.lens (\LakeFormationDataPermissionDetails' {lFTagPolicy} -> lFTagPolicy) (\s@LakeFormationDataPermissionDetails' {} a -> s {lFTagPolicy = a} :: LakeFormationDataPermissionDetails)

instance
  Data.FromJSON
    LakeFormationDataPermissionDetails
  where
  parseJSON =
    Data.withObject
      "LakeFormationDataPermissionDetails"
      ( \x ->
          LakeFormationDataPermissionDetails'
            Prelude.<$> (x Data..:? "LFTagPolicy")
      )

instance
  Prelude.Hashable
    LakeFormationDataPermissionDetails
  where
  hashWithSalt
    _salt
    LakeFormationDataPermissionDetails' {..} =
      _salt `Prelude.hashWithSalt` lFTagPolicy

instance
  Prelude.NFData
    LakeFormationDataPermissionDetails
  where
  rnf LakeFormationDataPermissionDetails' {..} =
    Prelude.rnf lFTagPolicy
