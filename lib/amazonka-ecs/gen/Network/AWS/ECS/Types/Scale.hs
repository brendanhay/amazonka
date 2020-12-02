{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Scale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Scale where

import Network.AWS.ECS.Types.ScaleUnit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
--
--
-- /See:/ 'scale' smart constructor.
data Scale = Scale'
  { _sValue :: !(Maybe Double),
    _sUnit :: !(Maybe ScaleUnit)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scale' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
--
-- * 'sUnit' - The unit of measure for the scale value.
scale ::
  Scale
scale = Scale' {_sValue = Nothing, _sUnit = Nothing}

-- | The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
sValue :: Lens' Scale (Maybe Double)
sValue = lens _sValue (\s a -> s {_sValue = a})

-- | The unit of measure for the scale value.
sUnit :: Lens' Scale (Maybe ScaleUnit)
sUnit = lens _sUnit (\s a -> s {_sUnit = a})

instance FromJSON Scale where
  parseJSON =
    withObject
      "Scale"
      (\x -> Scale' <$> (x .:? "value") <*> (x .:? "unit"))

instance Hashable Scale

instance NFData Scale

instance ToJSON Scale where
  toJSON Scale' {..} =
    object
      (catMaybes [("value" .=) <$> _sValue, ("unit" .=) <$> _sUnit])
