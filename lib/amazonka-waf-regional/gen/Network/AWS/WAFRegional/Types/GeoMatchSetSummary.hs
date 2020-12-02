{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the identifier and the name of the @GeoMatchSet@ .
--
--
--
-- /See:/ 'geoMatchSetSummary' smart constructor.
data GeoMatchSetSummary = GeoMatchSetSummary'
  { _gmssGeoMatchSetId ::
      !Text,
    _gmssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmssGeoMatchSetId' - The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
--
-- * 'gmssName' - A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
geoMatchSetSummary ::
  -- | 'gmssGeoMatchSetId'
  Text ->
  -- | 'gmssName'
  Text ->
  GeoMatchSetSummary
geoMatchSetSummary pGeoMatchSetId_ pName_ =
  GeoMatchSetSummary'
    { _gmssGeoMatchSetId = pGeoMatchSetId_,
      _gmssName = pName_
    }

-- | The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
gmssGeoMatchSetId :: Lens' GeoMatchSetSummary Text
gmssGeoMatchSetId = lens _gmssGeoMatchSetId (\s a -> s {_gmssGeoMatchSetId = a})

-- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
gmssName :: Lens' GeoMatchSetSummary Text
gmssName = lens _gmssName (\s a -> s {_gmssName = a})

instance FromJSON GeoMatchSetSummary where
  parseJSON =
    withObject
      "GeoMatchSetSummary"
      ( \x ->
          GeoMatchSetSummary' <$> (x .: "GeoMatchSetId") <*> (x .: "Name")
      )

instance Hashable GeoMatchSetSummary

instance NFData GeoMatchSetSummary
