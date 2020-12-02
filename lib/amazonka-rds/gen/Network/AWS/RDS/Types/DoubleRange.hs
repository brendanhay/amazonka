{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DoubleRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DoubleRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A range of double values.
--
--
--
-- /See:/ 'doubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { _drTo :: !(Maybe Double),
    _drFrom :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DoubleRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drTo' - The maximum value in the range.
--
-- * 'drFrom' - The minimum value in the range.
doubleRange ::
  DoubleRange
doubleRange = DoubleRange' {_drTo = Nothing, _drFrom = Nothing}

-- | The maximum value in the range.
drTo :: Lens' DoubleRange (Maybe Double)
drTo = lens _drTo (\s a -> s {_drTo = a})

-- | The minimum value in the range.
drFrom :: Lens' DoubleRange (Maybe Double)
drFrom = lens _drFrom (\s a -> s {_drFrom = a})

instance FromXML DoubleRange where
  parseXML x = DoubleRange' <$> (x .@? "To") <*> (x .@? "From")

instance Hashable DoubleRange

instance NFData DoubleRange
