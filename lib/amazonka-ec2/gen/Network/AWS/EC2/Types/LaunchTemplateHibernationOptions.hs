{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether an instance is configured for hibernation.
--
--
--
-- /See:/ 'launchTemplateHibernationOptions' smart constructor.
newtype LaunchTemplateHibernationOptions = LaunchTemplateHibernationOptions'
  { _lthoConfigured ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateHibernationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lthoConfigured' - If this parameter is set to @true@ , the instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
launchTemplateHibernationOptions ::
  LaunchTemplateHibernationOptions
launchTemplateHibernationOptions =
  LaunchTemplateHibernationOptions' {_lthoConfigured = Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
lthoConfigured :: Lens' LaunchTemplateHibernationOptions (Maybe Bool)
lthoConfigured = lens _lthoConfigured (\s a -> s {_lthoConfigured = a})

instance FromXML LaunchTemplateHibernationOptions where
  parseXML x =
    LaunchTemplateHibernationOptions' <$> (x .@? "configured")

instance Hashable LaunchTemplateHibernationOptions

instance NFData LaunchTemplateHibernationOptions
