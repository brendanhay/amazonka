{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.GeoMatchConstraint

-- | Specifies the type of update to perform to an 'GeoMatchSet' with 'UpdateGeoMatchSet' .
--
--
--
-- /See:/ 'geoMatchSetUpdate' smart constructor.
data GeoMatchSetUpdate = GeoMatchSetUpdate'
  { _gmsuAction ::
      !ChangeAction,
    _gmsuGeoMatchConstraint :: !GeoMatchConstraint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsuAction' - Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
--
-- * 'gmsuGeoMatchConstraint' - The country from which web requests originate that you want AWS WAF to search for.
geoMatchSetUpdate ::
  -- | 'gmsuAction'
  ChangeAction ->
  -- | 'gmsuGeoMatchConstraint'
  GeoMatchConstraint ->
  GeoMatchSetUpdate
geoMatchSetUpdate pAction_ pGeoMatchConstraint_ =
  GeoMatchSetUpdate'
    { _gmsuAction = pAction_,
      _gmsuGeoMatchConstraint = pGeoMatchConstraint_
    }

-- | Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
gmsuAction :: Lens' GeoMatchSetUpdate ChangeAction
gmsuAction = lens _gmsuAction (\s a -> s {_gmsuAction = a})

-- | The country from which web requests originate that you want AWS WAF to search for.
gmsuGeoMatchConstraint :: Lens' GeoMatchSetUpdate GeoMatchConstraint
gmsuGeoMatchConstraint = lens _gmsuGeoMatchConstraint (\s a -> s {_gmsuGeoMatchConstraint = a})

instance Hashable GeoMatchSetUpdate

instance NFData GeoMatchSetUpdate

instance ToJSON GeoMatchSetUpdate where
  toJSON GeoMatchSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _gmsuAction),
            Just ("GeoMatchConstraint" .= _gmsuGeoMatchConstraint)
          ]
      )
