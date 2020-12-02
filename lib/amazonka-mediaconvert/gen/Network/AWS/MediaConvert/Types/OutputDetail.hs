{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputDetail where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.VideoDetail
import Network.AWS.Prelude

-- | Details regarding output
--
-- /See:/ 'outputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { _odVideoDetails ::
      !(Maybe VideoDetail),
    _odDurationInMs :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odVideoDetails' - Contains details about the output's video stream
--
-- * 'odDurationInMs' - Duration in milliseconds
outputDetail ::
  OutputDetail
outputDetail =
  OutputDetail'
    { _odVideoDetails = Nothing,
      _odDurationInMs = Nothing
    }

-- | Contains details about the output's video stream
odVideoDetails :: Lens' OutputDetail (Maybe VideoDetail)
odVideoDetails = lens _odVideoDetails (\s a -> s {_odVideoDetails = a})

-- | Duration in milliseconds
odDurationInMs :: Lens' OutputDetail (Maybe Int)
odDurationInMs = lens _odDurationInMs (\s a -> s {_odDurationInMs = a})

instance FromJSON OutputDetail where
  parseJSON =
    withObject
      "OutputDetail"
      ( \x ->
          OutputDetail'
            <$> (x .:? "videoDetails") <*> (x .:? "durationInMs")
      )

instance Hashable OutputDetail

instance NFData OutputDetail
