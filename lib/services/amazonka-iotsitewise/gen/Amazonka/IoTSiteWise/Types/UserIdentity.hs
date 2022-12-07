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
-- Module      : Amazonka.IoTSiteWise.Types.UserIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.UserIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information for a user identity in an access policy.
--
-- /See:/ 'newUserIdentity' smart constructor.
data UserIdentity = UserIdentity'
  { -- | The IAM Identity Center ID of the user.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'userIdentity_id' - The IAM Identity Center ID of the user.
newUserIdentity ::
  -- | 'id'
  Prelude.Text ->
  UserIdentity
newUserIdentity pId_ = UserIdentity' {id = pId_}

-- | The IAM Identity Center ID of the user.
userIdentity_id :: Lens.Lens' UserIdentity Prelude.Text
userIdentity_id = Lens.lens (\UserIdentity' {id} -> id) (\s@UserIdentity' {} a -> s {id = a} :: UserIdentity)

instance Data.FromJSON UserIdentity where
  parseJSON =
    Data.withObject
      "UserIdentity"
      (\x -> UserIdentity' Prelude.<$> (x Data..: "id"))

instance Prelude.Hashable UserIdentity where
  hashWithSalt _salt UserIdentity' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData UserIdentity where
  rnf UserIdentity' {..} = Prelude.rnf id

instance Data.ToJSON UserIdentity where
  toJSON UserIdentity' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])
