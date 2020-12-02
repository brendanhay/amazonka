{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StatsEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StatsEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Stats

-- | Container for the Stats Event.
--
--
--
-- /See:/ 'statsEvent' smart constructor.
newtype StatsEvent = StatsEvent' {_seDetails :: Maybe Stats}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seDetails' - The Stats event details.
statsEvent ::
  StatsEvent
statsEvent = StatsEvent' {_seDetails = Nothing}

-- | The Stats event details.
seDetails :: Lens' StatsEvent (Maybe Stats)
seDetails = lens _seDetails (\s a -> s {_seDetails = a})

instance FromXML StatsEvent where
  parseXML x = StatsEvent' <$> (x .@? "Details")

instance Hashable StatsEvent

instance NFData StatsEvent
