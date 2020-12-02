{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RawSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RawSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Raw Settings
--
-- /See:/ 'rawSettings' smart constructor.
data RawSettings = RawSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RawSettings' with the minimum fields required to make a request.
rawSettings ::
  RawSettings
rawSettings = RawSettings'

instance FromJSON RawSettings where
  parseJSON = withObject "RawSettings" (\x -> pure RawSettings')

instance Hashable RawSettings

instance NFData RawSettings

instance ToJSON RawSettings where
  toJSON = const (Object mempty)
