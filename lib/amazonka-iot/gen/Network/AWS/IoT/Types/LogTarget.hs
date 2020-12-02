{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTarget where

import Network.AWS.IoT.Types.LogTargetType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A log target.
--
--
--
-- /See:/ 'logTarget' smart constructor.
data LogTarget = LogTarget'
  { _ltTargetName :: !(Maybe Text),
    _ltTargetType :: !LogTargetType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltTargetName' - The target name.
--
-- * 'ltTargetType' - The target type.
logTarget ::
  -- | 'ltTargetType'
  LogTargetType ->
  LogTarget
logTarget pTargetType_ =
  LogTarget' {_ltTargetName = Nothing, _ltTargetType = pTargetType_}

-- | The target name.
ltTargetName :: Lens' LogTarget (Maybe Text)
ltTargetName = lens _ltTargetName (\s a -> s {_ltTargetName = a})

-- | The target type.
ltTargetType :: Lens' LogTarget LogTargetType
ltTargetType = lens _ltTargetType (\s a -> s {_ltTargetType = a})

instance FromJSON LogTarget where
  parseJSON =
    withObject
      "LogTarget"
      ( \x ->
          LogTarget' <$> (x .:? "targetName") <*> (x .: "targetType")
      )

instance Hashable LogTarget

instance NFData LogTarget

instance ToJSON LogTarget where
  toJSON LogTarget' {..} =
    object
      ( catMaybes
          [ ("targetName" .=) <$> _ltTargetName,
            Just ("targetType" .= _ltTargetType)
          ]
      )
