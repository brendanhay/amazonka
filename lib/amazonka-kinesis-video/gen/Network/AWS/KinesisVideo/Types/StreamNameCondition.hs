{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.StreamNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamNameCondition where

import Network.AWS.KinesisVideo.Types.ComparisonOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the condition that streams must satisfy to be returned when you list streams (see the @ListStreams@ API). A condition has a comparison operation and a value. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
--
--
-- /See:/ 'streamNameCondition' smart constructor.
data StreamNameCondition = StreamNameCondition'
  { _sncComparisonOperator ::
      !(Maybe ComparisonOperator),
    _sncComparisonValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamNameCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sncComparisonOperator' - A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- * 'sncComparisonValue' - A value to compare.
streamNameCondition ::
  StreamNameCondition
streamNameCondition =
  StreamNameCondition'
    { _sncComparisonOperator = Nothing,
      _sncComparisonValue = Nothing
    }

-- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
sncComparisonOperator :: Lens' StreamNameCondition (Maybe ComparisonOperator)
sncComparisonOperator = lens _sncComparisonOperator (\s a -> s {_sncComparisonOperator = a})

-- | A value to compare.
sncComparisonValue :: Lens' StreamNameCondition (Maybe Text)
sncComparisonValue = lens _sncComparisonValue (\s a -> s {_sncComparisonValue = a})

instance Hashable StreamNameCondition

instance NFData StreamNameCondition

instance ToJSON StreamNameCondition where
  toJSON StreamNameCondition' {..} =
    object
      ( catMaybes
          [ ("ComparisonOperator" .=) <$> _sncComparisonOperator,
            ("ComparisonValue" .=) <$> _sncComparisonValue
          ]
      )
