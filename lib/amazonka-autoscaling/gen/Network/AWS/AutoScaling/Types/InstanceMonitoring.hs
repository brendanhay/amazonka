{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMonitoring where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether detailed monitoring is enabled for the Auto Scaling instances.
--
--
--
-- /See:/ 'instanceMonitoring' smart constructor.
newtype InstanceMonitoring = InstanceMonitoring'
  { _imEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imEnabled' - If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
instanceMonitoring ::
  InstanceMonitoring
instanceMonitoring = InstanceMonitoring' {_imEnabled = Nothing}

-- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\s a -> s {_imEnabled = a})

instance FromXML InstanceMonitoring where
  parseXML x = InstanceMonitoring' <$> (x .@? "Enabled")

instance Hashable InstanceMonitoring

instance NFData InstanceMonitoring

instance ToQuery InstanceMonitoring where
  toQuery InstanceMonitoring' {..} = mconcat ["Enabled" =: _imEnabled]
