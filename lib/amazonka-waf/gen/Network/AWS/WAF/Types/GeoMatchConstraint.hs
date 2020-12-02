{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraint where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.GeoMatchConstraintType
import Network.AWS.WAF.Types.GeoMatchConstraintValue

-- | The country from which web requests originate that you want AWS WAF to search for.
--
--
--
-- /See:/ 'geoMatchConstraint' smart constructor.
data GeoMatchConstraint = GeoMatchConstraint'
  { _gmcType ::
      !GeoMatchConstraintType,
    _gmcValue :: !GeoMatchConstraintValue
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GeoMatchConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcType' - The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
--
-- * 'gmcValue' - The country that you want AWS WAF to search for.
geoMatchConstraint ::
  -- | 'gmcType'
  GeoMatchConstraintType ->
  -- | 'gmcValue'
  GeoMatchConstraintValue ->
  GeoMatchConstraint
geoMatchConstraint pType_ pValue_ =
  GeoMatchConstraint' {_gmcType = pType_, _gmcValue = pValue_}

-- | The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
gmcType :: Lens' GeoMatchConstraint GeoMatchConstraintType
gmcType = lens _gmcType (\s a -> s {_gmcType = a})

-- | The country that you want AWS WAF to search for.
gmcValue :: Lens' GeoMatchConstraint GeoMatchConstraintValue
gmcValue = lens _gmcValue (\s a -> s {_gmcValue = a})

instance FromJSON GeoMatchConstraint where
  parseJSON =
    withObject
      "GeoMatchConstraint"
      (\x -> GeoMatchConstraint' <$> (x .: "Type") <*> (x .: "Value"))

instance Hashable GeoMatchConstraint

instance NFData GeoMatchConstraint

instance ToJSON GeoMatchConstraint where
  toJSON GeoMatchConstraint' {..} =
    object
      ( catMaybes
          [Just ("Type" .= _gmcType), Just ("Value" .= _gmcValue)]
      )
