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
-- Module      : Amazonka.CloudFront.Types.QueryArgProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.QueryArgProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'newQueryArgProfile' smart constructor.
data QueryArgProfile = QueryArgProfile'
  { -- | Query argument for field-level encryption query argument-profile
    -- mapping.
    queryArg :: Prelude.Text,
    -- | ID of profile to use for field-level encryption query argument-profile
    -- mapping
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryArgProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryArg', 'queryArgProfile_queryArg' - Query argument for field-level encryption query argument-profile
-- mapping.
--
-- 'profileId', 'queryArgProfile_profileId' - ID of profile to use for field-level encryption query argument-profile
-- mapping
newQueryArgProfile ::
  -- | 'queryArg'
  Prelude.Text ->
  -- | 'profileId'
  Prelude.Text ->
  QueryArgProfile
newQueryArgProfile pQueryArg_ pProfileId_ =
  QueryArgProfile'
    { queryArg = pQueryArg_,
      profileId = pProfileId_
    }

-- | Query argument for field-level encryption query argument-profile
-- mapping.
queryArgProfile_queryArg :: Lens.Lens' QueryArgProfile Prelude.Text
queryArgProfile_queryArg = Lens.lens (\QueryArgProfile' {queryArg} -> queryArg) (\s@QueryArgProfile' {} a -> s {queryArg = a} :: QueryArgProfile)

-- | ID of profile to use for field-level encryption query argument-profile
-- mapping
queryArgProfile_profileId :: Lens.Lens' QueryArgProfile Prelude.Text
queryArgProfile_profileId = Lens.lens (\QueryArgProfile' {profileId} -> profileId) (\s@QueryArgProfile' {} a -> s {profileId = a} :: QueryArgProfile)

instance Data.FromXML QueryArgProfile where
  parseXML x =
    QueryArgProfile'
      Prelude.<$> (x Data..@ "QueryArg")
      Prelude.<*> (x Data..@ "ProfileId")

instance Prelude.Hashable QueryArgProfile where
  hashWithSalt _salt QueryArgProfile' {..} =
    _salt
      `Prelude.hashWithSalt` queryArg
      `Prelude.hashWithSalt` profileId

instance Prelude.NFData QueryArgProfile where
  rnf QueryArgProfile' {..} =
    Prelude.rnf queryArg `Prelude.seq`
      Prelude.rnf profileId

instance Data.ToXML QueryArgProfile where
  toXML QueryArgProfile' {..} =
    Prelude.mconcat
      [ "QueryArg" Data.@= queryArg,
        "ProfileId" Data.@= profileId
      ]
