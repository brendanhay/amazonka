{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Restrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Restrictions where

import Network.AWS.CloudFront.Types.GeoRestriction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
--
--
-- /See:/ 'restrictions' smart constructor.
newtype Restrictions = Restrictions'
  { _rGeoRestriction ::
      GeoRestriction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Restrictions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rGeoRestriction' - A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
restrictions ::
  -- | 'rGeoRestriction'
  GeoRestriction ->
  Restrictions
restrictions pGeoRestriction_ =
  Restrictions' {_rGeoRestriction = pGeoRestriction_}

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
rGeoRestriction :: Lens' Restrictions GeoRestriction
rGeoRestriction = lens _rGeoRestriction (\s a -> s {_rGeoRestriction = a})

instance FromXML Restrictions where
  parseXML x = Restrictions' <$> (x .@ "GeoRestriction")

instance Hashable Restrictions

instance NFData Restrictions

instance ToXML Restrictions where
  toXML Restrictions' {..} =
    mconcat ["GeoRestriction" @= _rGeoRestriction]
