{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngressAccessLogs where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configure ingress access logging.
--
-- /See:/ 'ingressAccessLogs' smart constructor.
newtype IngressAccessLogs = IngressAccessLogs'
  { _ialLogGroupName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IngressAccessLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ialLogGroupName' - Customize the log group name.
ingressAccessLogs ::
  IngressAccessLogs
ingressAccessLogs = IngressAccessLogs' {_ialLogGroupName = Nothing}

-- | Customize the log group name.
ialLogGroupName :: Lens' IngressAccessLogs (Maybe Text)
ialLogGroupName = lens _ialLogGroupName (\s a -> s {_ialLogGroupName = a})

instance FromJSON IngressAccessLogs where
  parseJSON =
    withObject
      "IngressAccessLogs"
      (\x -> IngressAccessLogs' <$> (x .:? "logGroupName"))

instance Hashable IngressAccessLogs

instance NFData IngressAccessLogs

instance ToJSON IngressAccessLogs where
  toJSON IngressAccessLogs' {..} =
    object (catMaybes [("logGroupName" .=) <$> _ialLogGroupName])
