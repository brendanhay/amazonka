{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.EgressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.EgressAccessLogs where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configure egress access logging.
--
-- /See:/ 'egressAccessLogs' smart constructor.
newtype EgressAccessLogs = EgressAccessLogs'
  { _ealLogGroupName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EgressAccessLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ealLogGroupName' - Customize the log group name.
egressAccessLogs ::
  EgressAccessLogs
egressAccessLogs = EgressAccessLogs' {_ealLogGroupName = Nothing}

-- | Customize the log group name.
ealLogGroupName :: Lens' EgressAccessLogs (Maybe Text)
ealLogGroupName = lens _ealLogGroupName (\s a -> s {_ealLogGroupName = a})

instance FromJSON EgressAccessLogs where
  parseJSON =
    withObject
      "EgressAccessLogs"
      (\x -> EgressAccessLogs' <$> (x .:? "logGroupName"))

instance Hashable EgressAccessLogs

instance NFData EgressAccessLogs

instance ToJSON EgressAccessLogs where
  toJSON EgressAccessLogs' {..} =
    object (catMaybes [("logGroupName" .=) <$> _ealLogGroupName])
