{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfile where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML QueryArgProfile where
  parseXML x =
    QueryArgProfile'
      Prelude.<$> (x Prelude..@ "QueryArg")
      Prelude.<*> (x Prelude..@ "ProfileId")

instance Prelude.Hashable QueryArgProfile

instance Prelude.NFData QueryArgProfile

instance Prelude.ToXML QueryArgProfile where
  toXML QueryArgProfile' {..} =
    Prelude.mconcat
      [ "QueryArg" Prelude.@= queryArg,
        "ProfileId" Prelude.@= profileId
      ]
