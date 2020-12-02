{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec709Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec709Settings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Rec709 Settings
--
-- /See:/ 'rec709Settings' smart constructor.
data Rec709Settings = Rec709Settings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rec709Settings' with the minimum fields required to make a request.
rec709Settings ::
  Rec709Settings
rec709Settings = Rec709Settings'

instance FromJSON Rec709Settings where
  parseJSON =
    withObject "Rec709Settings" (\x -> pure Rec709Settings')

instance Hashable Rec709Settings

instance NFData Rec709Settings

instance ToJSON Rec709Settings where
  toJSON = const (Object mempty)
