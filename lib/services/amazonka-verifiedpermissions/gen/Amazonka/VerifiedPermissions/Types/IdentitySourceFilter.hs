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
-- Module      : Amazonka.VerifiedPermissions.Types.IdentitySourceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.IdentitySourceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines characteristics of an identity source that you
-- can use to filter.
--
-- This data type is used as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListIdentityStores.html ListIdentityStores>
-- operation.
--
-- /See:/ 'newIdentitySourceFilter' smart constructor.
data IdentitySourceFilter = IdentitySourceFilter'
  { -- | The Cedar entity type of the principals returned by the identity
    -- provider (IdP) associated with this identity source.
    principalEntityType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentitySourceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalEntityType', 'identitySourceFilter_principalEntityType' - The Cedar entity type of the principals returned by the identity
-- provider (IdP) associated with this identity source.
newIdentitySourceFilter ::
  IdentitySourceFilter
newIdentitySourceFilter =
  IdentitySourceFilter'
    { principalEntityType =
        Prelude.Nothing
    }

-- | The Cedar entity type of the principals returned by the identity
-- provider (IdP) associated with this identity source.
identitySourceFilter_principalEntityType :: Lens.Lens' IdentitySourceFilter (Prelude.Maybe Prelude.Text)
identitySourceFilter_principalEntityType = Lens.lens (\IdentitySourceFilter' {principalEntityType} -> principalEntityType) (\s@IdentitySourceFilter' {} a -> s {principalEntityType = a} :: IdentitySourceFilter)

instance Prelude.Hashable IdentitySourceFilter where
  hashWithSalt _salt IdentitySourceFilter' {..} =
    _salt `Prelude.hashWithSalt` principalEntityType

instance Prelude.NFData IdentitySourceFilter where
  rnf IdentitySourceFilter' {..} =
    Prelude.rnf principalEntityType

instance Data.ToJSON IdentitySourceFilter where
  toJSON IdentitySourceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("principalEntityType" Data..=)
              Prelude.<$> principalEntityType
          ]
      )
