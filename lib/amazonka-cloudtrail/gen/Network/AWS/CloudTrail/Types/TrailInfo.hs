{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.TrailInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.TrailInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a CloudTrail trail, including the trail's name, home region, and Amazon Resource Name (ARN).
--
--
--
-- /See:/ 'trailInfo' smart constructor.
data TrailInfo = TrailInfo'
  { _tiTrailARN :: !(Maybe Text),
    _tiHomeRegion :: !(Maybe Text),
    _tiName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrailInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiTrailARN' - The ARN of a trail.
--
-- * 'tiHomeRegion' - The AWS region in which a trail was created.
--
-- * 'tiName' - The name of a trail.
trailInfo ::
  TrailInfo
trailInfo =
  TrailInfo'
    { _tiTrailARN = Nothing,
      _tiHomeRegion = Nothing,
      _tiName = Nothing
    }

-- | The ARN of a trail.
tiTrailARN :: Lens' TrailInfo (Maybe Text)
tiTrailARN = lens _tiTrailARN (\s a -> s {_tiTrailARN = a})

-- | The AWS region in which a trail was created.
tiHomeRegion :: Lens' TrailInfo (Maybe Text)
tiHomeRegion = lens _tiHomeRegion (\s a -> s {_tiHomeRegion = a})

-- | The name of a trail.
tiName :: Lens' TrailInfo (Maybe Text)
tiName = lens _tiName (\s a -> s {_tiName = a})

instance FromJSON TrailInfo where
  parseJSON =
    withObject
      "TrailInfo"
      ( \x ->
          TrailInfo'
            <$> (x .:? "TrailARN") <*> (x .:? "HomeRegion") <*> (x .:? "Name")
      )

instance Hashable TrailInfo

instance NFData TrailInfo
