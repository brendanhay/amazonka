{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec601Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec601Settings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Rec601 Settings
--
-- /See:/ 'rec601Settings' smart constructor.
data Rec601Settings = Rec601Settings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rec601Settings' with the minimum fields required to make a request.
rec601Settings ::
  Rec601Settings
rec601Settings = Rec601Settings'

instance FromJSON Rec601Settings where
  parseJSON =
    withObject "Rec601Settings" (\x -> pure Rec601Settings')

instance Hashable Rec601Settings

instance NFData Rec601Settings

instance ToJSON Rec601Settings where
  toJSON = const (Object mempty)
