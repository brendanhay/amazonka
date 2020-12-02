{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.FilterActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.FilterActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that filters a message based on its attributes.
--
--
--
-- /See:/ 'filterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { _faNext :: !(Maybe Text),
    _faName :: !Text,
    _faFilter :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FilterActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faNext' - The next activity in the pipeline.
--
-- * 'faName' - The name of the filter activity.
--
-- * 'faFilter' - An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity.
filterActivity ::
  -- | 'faName'
  Text ->
  -- | 'faFilter'
  Text ->
  FilterActivity
filterActivity pName_ pFilter_ =
  FilterActivity'
    { _faNext = Nothing,
      _faName = pName_,
      _faFilter = pFilter_
    }

-- | The next activity in the pipeline.
faNext :: Lens' FilterActivity (Maybe Text)
faNext = lens _faNext (\s a -> s {_faNext = a})

-- | The name of the filter activity.
faName :: Lens' FilterActivity Text
faName = lens _faName (\s a -> s {_faName = a})

-- | An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity.
faFilter :: Lens' FilterActivity Text
faFilter = lens _faFilter (\s a -> s {_faFilter = a})

instance FromJSON FilterActivity where
  parseJSON =
    withObject
      "FilterActivity"
      ( \x ->
          FilterActivity'
            <$> (x .:? "next") <*> (x .: "name") <*> (x .: "filter")
      )

instance Hashable FilterActivity

instance NFData FilterActivity

instance ToJSON FilterActivity where
  toJSON FilterActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _faNext,
            Just ("name" .= _faName),
            Just ("filter" .= _faFilter)
          ]
      )
