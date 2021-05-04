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
-- Module      : Network.AWS.CloudFront.Types.Restrictions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Restrictions where

import Network.AWS.CloudFront.Types.GeoRestriction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- /See:/ 'newRestrictions' smart constructor.
data Restrictions = Restrictions'
  { -- | A complex type that controls the countries in which your content is
    -- distributed. CloudFront determines the location of your users using
    -- @MaxMind@ GeoIP databases.
    geoRestriction :: GeoRestriction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Restrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoRestriction', 'restrictions_geoRestriction' - A complex type that controls the countries in which your content is
-- distributed. CloudFront determines the location of your users using
-- @MaxMind@ GeoIP databases.
newRestrictions ::
  -- | 'geoRestriction'
  GeoRestriction ->
  Restrictions
newRestrictions pGeoRestriction_ =
  Restrictions' {geoRestriction = pGeoRestriction_}

-- | A complex type that controls the countries in which your content is
-- distributed. CloudFront determines the location of your users using
-- @MaxMind@ GeoIP databases.
restrictions_geoRestriction :: Lens.Lens' Restrictions GeoRestriction
restrictions_geoRestriction = Lens.lens (\Restrictions' {geoRestriction} -> geoRestriction) (\s@Restrictions' {} a -> s {geoRestriction = a} :: Restrictions)

instance Prelude.FromXML Restrictions where
  parseXML x =
    Restrictions'
      Prelude.<$> (x Prelude..@ "GeoRestriction")

instance Prelude.Hashable Restrictions

instance Prelude.NFData Restrictions

instance Prelude.ToXML Restrictions where
  toXML Restrictions' {..} =
    Prelude.mconcat
      ["GeoRestriction" Prelude.@= geoRestriction]
