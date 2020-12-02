{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelNameCondition where

import Network.AWS.KinesisVideo.Types.ComparisonOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An optional input parameter for the @ListSignalingChannels@ API. When this parameter is specified while invoking @ListSignalingChannels@ , the API returns only the channels that satisfy a condition specified in @ChannelNameCondition@ .
--
--
--
-- /See:/ 'channelNameCondition' smart constructor.
data ChannelNameCondition = ChannelNameCondition'
  { _cncComparisonOperator ::
      !(Maybe ComparisonOperator),
    _cncComparisonValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelNameCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cncComparisonOperator' - A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
--
-- * 'cncComparisonValue' - A value to compare.
channelNameCondition ::
  ChannelNameCondition
channelNameCondition =
  ChannelNameCondition'
    { _cncComparisonOperator = Nothing,
      _cncComparisonValue = Nothing
    }

-- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
cncComparisonOperator :: Lens' ChannelNameCondition (Maybe ComparisonOperator)
cncComparisonOperator = lens _cncComparisonOperator (\s a -> s {_cncComparisonOperator = a})

-- | A value to compare.
cncComparisonValue :: Lens' ChannelNameCondition (Maybe Text)
cncComparisonValue = lens _cncComparisonValue (\s a -> s {_cncComparisonValue = a})

instance Hashable ChannelNameCondition

instance NFData ChannelNameCondition

instance ToJSON ChannelNameCondition where
  toJSON ChannelNameCondition' {..} =
    object
      ( catMaybes
          [ ("ComparisonOperator" .=) <$> _cncComparisonOperator,
            ("ComparisonValue" .=) <$> _cncComparisonValue
          ]
      )
