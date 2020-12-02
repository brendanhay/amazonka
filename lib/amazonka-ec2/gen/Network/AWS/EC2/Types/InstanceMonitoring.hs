{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMonitoring where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'instanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { _imInstanceId ::
      !(Maybe Text),
    _imMonitoring :: !(Maybe Monitoring)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imInstanceId' - The ID of the instance.
--
-- * 'imMonitoring' - The monitoring for the instance.
instanceMonitoring ::
  InstanceMonitoring
instanceMonitoring =
  InstanceMonitoring'
    { _imInstanceId = Nothing,
      _imMonitoring = Nothing
    }

-- | The ID of the instance.
imInstanceId :: Lens' InstanceMonitoring (Maybe Text)
imInstanceId = lens _imInstanceId (\s a -> s {_imInstanceId = a})

-- | The monitoring for the instance.
imMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
imMonitoring = lens _imMonitoring (\s a -> s {_imMonitoring = a})

instance FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      <$> (x .@? "instanceId") <*> (x .@? "monitoring")

instance Hashable InstanceMonitoring

instance NFData InstanceMonitoring
