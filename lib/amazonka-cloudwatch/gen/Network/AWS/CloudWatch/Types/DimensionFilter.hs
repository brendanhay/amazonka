{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DimensionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DimensionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents filters for a dimension.
--
--
--
-- /See:/ 'dimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { _dfValue :: !(Maybe Text),
    _dfName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DimensionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfValue' - The value of the dimension to be matched.
--
-- * 'dfName' - The dimension name to be matched.
dimensionFilter ::
  -- | 'dfName'
  Text ->
  DimensionFilter
dimensionFilter pName_ =
  DimensionFilter' {_dfValue = Nothing, _dfName = pName_}

-- | The value of the dimension to be matched.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\s a -> s {_dfValue = a})

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\s a -> s {_dfName = a})

instance Hashable DimensionFilter

instance NFData DimensionFilter

instance ToQuery DimensionFilter where
  toQuery DimensionFilter' {..} =
    mconcat ["Value" =: _dfValue, "Name" =: _dfName]
