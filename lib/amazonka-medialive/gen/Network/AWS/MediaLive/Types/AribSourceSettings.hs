{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AribSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribSourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Arib Source Settings
--
-- /See:/ 'aribSourceSettings' smart constructor.
data AribSourceSettings = AribSourceSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AribSourceSettings' with the minimum fields required to make a request.
aribSourceSettings ::
  AribSourceSettings
aribSourceSettings = AribSourceSettings'

instance FromJSON AribSourceSettings where
  parseJSON =
    withObject "AribSourceSettings" (\x -> pure AribSourceSettings')

instance Hashable AribSourceSettings

instance NFData AribSourceSettings

instance ToJSON AribSourceSettings where
  toJSON = const (Object mempty)
