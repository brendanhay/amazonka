{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Auto Scaling launch configuration.
--
--
--
-- /See:/ 'autoScalingGroup' smart constructor.
newtype AutoScalingGroup = AutoScalingGroup'
  { _asgName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgName' - The name of the @AutoScalingGroup@ .
autoScalingGroup ::
  AutoScalingGroup
autoScalingGroup = AutoScalingGroup' {_asgName = Nothing}

-- | The name of the @AutoScalingGroup@ .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\s a -> s {_asgName = a})

instance FromXML AutoScalingGroup where
  parseXML x = AutoScalingGroup' <$> (x .@? "Name")

instance Hashable AutoScalingGroup

instance NFData AutoScalingGroup
