{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Range where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A range of integer values.
--
--
--
-- /See:/ 'range' smart constructor.
data Range = Range'
  { _rTo :: !(Maybe Int),
    _rFrom :: !(Maybe Int),
    _rStep :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rTo' - The maximum value in the range.
--
-- * 'rFrom' - The minimum value in the range.
--
-- * 'rStep' - The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
range ::
  Range
range = Range' {_rTo = Nothing, _rFrom = Nothing, _rStep = Nothing}

-- | The maximum value in the range.
rTo :: Lens' Range (Maybe Int)
rTo = lens _rTo (\s a -> s {_rTo = a})

-- | The minimum value in the range.
rFrom :: Lens' Range (Maybe Int)
rFrom = lens _rFrom (\s a -> s {_rFrom = a})

-- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
rStep :: Lens' Range (Maybe Int)
rStep = lens _rStep (\s a -> s {_rStep = a})

instance FromXML Range where
  parseXML x =
    Range' <$> (x .@? "To") <*> (x .@? "From") <*> (x .@? "Step")

instance Hashable Range

instance NFData Range
