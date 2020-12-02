{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Auto Scaling launch configuration.
--
--
--
-- /See:/ 'launchConfiguration' smart constructor.
newtype LaunchConfiguration = LaunchConfiguration'
  { _lcName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcName' - The name of the launch configuration.
launchConfiguration ::
  LaunchConfiguration
launchConfiguration = LaunchConfiguration' {_lcName = Nothing}

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\s a -> s {_lcName = a})

instance FromXML LaunchConfiguration where
  parseXML x = LaunchConfiguration' <$> (x .@? "Name")

instance Hashable LaunchConfiguration

instance NFData LaunchConfiguration
