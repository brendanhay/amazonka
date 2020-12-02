{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelism
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputParallelism where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'inputParallelism' smart constructor.
newtype InputParallelism = InputParallelism' {_ipCount :: Maybe Nat}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputParallelism' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipCount' - Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
inputParallelism ::
  InputParallelism
inputParallelism = InputParallelism' {_ipCount = Nothing}

-- | Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
ipCount :: Lens' InputParallelism (Maybe Natural)
ipCount = lens _ipCount (\s a -> s {_ipCount = a}) . mapping _Nat

instance FromJSON InputParallelism where
  parseJSON =
    withObject
      "InputParallelism"
      (\x -> InputParallelism' <$> (x .:? "Count"))

instance Hashable InputParallelism

instance NFData InputParallelism

instance ToJSON InputParallelism where
  toJSON InputParallelism' {..} =
    object (catMaybes [("Count" .=) <$> _ipCount])
