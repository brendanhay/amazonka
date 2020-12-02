{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Impact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The anomaly's dollar value.
--
--
--
-- /See:/ 'impact' smart constructor.
data Impact = Impact'
  { _iTotalImpact :: !(Maybe Double),
    _iMaxImpact :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Impact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iTotalImpact' - The cumulative dollar value observed for an anomaly.
--
-- * 'iMaxImpact' - The maximum dollar value observed for an anomaly.
impact ::
  -- | 'iMaxImpact'
  Double ->
  Impact
impact pMaxImpact_ =
  Impact' {_iTotalImpact = Nothing, _iMaxImpact = pMaxImpact_}

-- | The cumulative dollar value observed for an anomaly.
iTotalImpact :: Lens' Impact (Maybe Double)
iTotalImpact = lens _iTotalImpact (\s a -> s {_iTotalImpact = a})

-- | The maximum dollar value observed for an anomaly.
iMaxImpact :: Lens' Impact Double
iMaxImpact = lens _iMaxImpact (\s a -> s {_iMaxImpact = a})

instance FromJSON Impact where
  parseJSON =
    withObject
      "Impact"
      (\x -> Impact' <$> (x .:? "TotalImpact") <*> (x .: "MaxImpact"))

instance Hashable Impact

instance NFData Impact
