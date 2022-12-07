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
-- Module      : Amazonka.RolesAnywhere.Types.ScalarProfileRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ScalarProfileRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newScalarProfileRequest' smart constructor.
data ScalarProfileRequest = ScalarProfileRequest'
  { -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalarProfileRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'scalarProfileRequest_profileId' - The unique identifier of the profile.
newScalarProfileRequest ::
  -- | 'profileId'
  Prelude.Text ->
  ScalarProfileRequest
newScalarProfileRequest pProfileId_ =
  ScalarProfileRequest' {profileId = pProfileId_}

-- | The unique identifier of the profile.
scalarProfileRequest_profileId :: Lens.Lens' ScalarProfileRequest Prelude.Text
scalarProfileRequest_profileId = Lens.lens (\ScalarProfileRequest' {profileId} -> profileId) (\s@ScalarProfileRequest' {} a -> s {profileId = a} :: ScalarProfileRequest)

instance Prelude.Hashable ScalarProfileRequest where
  hashWithSalt _salt ScalarProfileRequest' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData ScalarProfileRequest where
  rnf ScalarProfileRequest' {..} = Prelude.rnf profileId

instance Data.ToJSON ScalarProfileRequest where
  toJSON ScalarProfileRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("profileId" Data..= profileId)]
      )
