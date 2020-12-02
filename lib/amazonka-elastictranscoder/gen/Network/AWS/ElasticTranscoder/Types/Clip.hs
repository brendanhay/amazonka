{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Clip
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Clip where

import Network.AWS.ElasticTranscoder.Types.TimeSpan
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.
--
--
--
-- /See:/ 'clip' smart constructor.
newtype Clip = Clip' {_cTimeSpan :: Maybe TimeSpan}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Clip' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cTimeSpan' - Settings that determine when a clip begins and how long it lasts.
clip ::
  Clip
clip = Clip' {_cTimeSpan = Nothing}

-- | Settings that determine when a clip begins and how long it lasts.
cTimeSpan :: Lens' Clip (Maybe TimeSpan)
cTimeSpan = lens _cTimeSpan (\s a -> s {_cTimeSpan = a})

instance FromJSON Clip where
  parseJSON = withObject "Clip" (\x -> Clip' <$> (x .:? "TimeSpan"))

instance Hashable Clip

instance NFData Clip

instance ToJSON Clip where
  toJSON Clip' {..} =
    object (catMaybes [("TimeSpan" .=) <$> _cTimeSpan])
