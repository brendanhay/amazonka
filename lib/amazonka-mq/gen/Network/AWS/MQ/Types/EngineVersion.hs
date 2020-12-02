{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EngineVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Id of the engine version.
--
-- /See:/ 'engineVersion' smart constructor.
newtype EngineVersion = EngineVersion' {_evName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evName' - Id for the version.
engineVersion ::
  EngineVersion
engineVersion = EngineVersion' {_evName = Nothing}

-- | Id for the version.
evName :: Lens' EngineVersion (Maybe Text)
evName = lens _evName (\s a -> s {_evName = a})

instance FromJSON EngineVersion where
  parseJSON =
    withObject
      "EngineVersion"
      (\x -> EngineVersion' <$> (x .:? "name"))

instance Hashable EngineVersion

instance NFData EngineVersion
