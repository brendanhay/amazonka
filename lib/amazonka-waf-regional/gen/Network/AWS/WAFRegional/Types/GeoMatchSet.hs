{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.GeoMatchConstraint

-- | Contains one or more countries that AWS WAF will search for.
--
--
--
-- /See:/ 'geoMatchSet' smart constructor.
data GeoMatchSet = GeoMatchSet'
  { _gmsName :: !(Maybe Text),
    _gmsGeoMatchSetId :: !Text,
    _gmsGeoMatchConstraints :: ![GeoMatchConstraint]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsName' - A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
--
-- * 'gmsGeoMatchSetId' - The @GeoMatchSetId@ for an @GeoMatchSet@ . You use @GeoMatchSetId@ to get information about a @GeoMatchSet@ (see 'GeoMatchSet' ), update a @GeoMatchSet@ (see 'UpdateGeoMatchSet' ), insert a @GeoMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @GeoMatchSet@ from AWS WAF (see 'DeleteGeoMatchSet' ). @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- * 'gmsGeoMatchConstraints' - An array of 'GeoMatchConstraint' objects, which contain the country that you want AWS WAF to search for.
geoMatchSet ::
  -- | 'gmsGeoMatchSetId'
  Text ->
  GeoMatchSet
geoMatchSet pGeoMatchSetId_ =
  GeoMatchSet'
    { _gmsName = Nothing,
      _gmsGeoMatchSetId = pGeoMatchSetId_,
      _gmsGeoMatchConstraints = mempty
    }

-- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
gmsName :: Lens' GeoMatchSet (Maybe Text)
gmsName = lens _gmsName (\s a -> s {_gmsName = a})

-- | The @GeoMatchSetId@ for an @GeoMatchSet@ . You use @GeoMatchSetId@ to get information about a @GeoMatchSet@ (see 'GeoMatchSet' ), update a @GeoMatchSet@ (see 'UpdateGeoMatchSet' ), insert a @GeoMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @GeoMatchSet@ from AWS WAF (see 'DeleteGeoMatchSet' ). @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
gmsGeoMatchSetId :: Lens' GeoMatchSet Text
gmsGeoMatchSetId = lens _gmsGeoMatchSetId (\s a -> s {_gmsGeoMatchSetId = a})

-- | An array of 'GeoMatchConstraint' objects, which contain the country that you want AWS WAF to search for.
gmsGeoMatchConstraints :: Lens' GeoMatchSet [GeoMatchConstraint]
gmsGeoMatchConstraints = lens _gmsGeoMatchConstraints (\s a -> s {_gmsGeoMatchConstraints = a}) . _Coerce

instance FromJSON GeoMatchSet where
  parseJSON =
    withObject
      "GeoMatchSet"
      ( \x ->
          GeoMatchSet'
            <$> (x .:? "Name")
            <*> (x .: "GeoMatchSetId")
            <*> (x .:? "GeoMatchConstraints" .!= mempty)
      )

instance Hashable GeoMatchSet

instance NFData GeoMatchSet
