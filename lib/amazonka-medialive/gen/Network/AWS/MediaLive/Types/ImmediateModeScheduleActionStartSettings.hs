{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'immediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImmediateModeScheduleActionStartSettings' with the minimum fields required to make a request.
immediateModeScheduleActionStartSettings ::
  ImmediateModeScheduleActionStartSettings
immediateModeScheduleActionStartSettings =
  ImmediateModeScheduleActionStartSettings'

instance FromJSON ImmediateModeScheduleActionStartSettings where
  parseJSON =
    withObject
      "ImmediateModeScheduleActionStartSettings"
      (\x -> pure ImmediateModeScheduleActionStartSettings')

instance Hashable ImmediateModeScheduleActionStartSettings

instance NFData ImmediateModeScheduleActionStartSettings

instance ToJSON ImmediateModeScheduleActionStartSettings where
  toJSON = const (Object mempty)
