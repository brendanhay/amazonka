{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys

-- | Filter for the DescribeActivation API.
--
--
--
-- /See:/ 'describeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { _dafFilterKey ::
      !(Maybe DescribeActivationsFilterKeys),
    _dafFilterValues :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeActivationsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dafFilterKey' - The name of the filter.
--
-- * 'dafFilterValues' - The filter values.
describeActivationsFilter ::
  DescribeActivationsFilter
describeActivationsFilter =
  DescribeActivationsFilter'
    { _dafFilterKey = Nothing,
      _dafFilterValues = Nothing
    }

-- | The name of the filter.
dafFilterKey :: Lens' DescribeActivationsFilter (Maybe DescribeActivationsFilterKeys)
dafFilterKey = lens _dafFilterKey (\s a -> s {_dafFilterKey = a})

-- | The filter values.
dafFilterValues :: Lens' DescribeActivationsFilter [Text]
dafFilterValues = lens _dafFilterValues (\s a -> s {_dafFilterValues = a}) . _Default . _Coerce

instance Hashable DescribeActivationsFilter

instance NFData DescribeActivationsFilter

instance ToJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter' {..} =
    object
      ( catMaybes
          [ ("FilterKey" .=) <$> _dafFilterKey,
            ("FilterValues" .=) <$> _dafFilterValues
          ]
      )
