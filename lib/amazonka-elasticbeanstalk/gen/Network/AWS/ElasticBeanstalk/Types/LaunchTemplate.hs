{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchTemplate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon EC2 launch template.
--
--
--
-- /See:/ 'launchTemplate' smart constructor.
newtype LaunchTemplate = LaunchTemplate' {_ltId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltId' - The ID of the launch template.
launchTemplate ::
  LaunchTemplate
launchTemplate = LaunchTemplate' {_ltId = Nothing}

-- | The ID of the launch template.
ltId :: Lens' LaunchTemplate (Maybe Text)
ltId = lens _ltId (\s a -> s {_ltId = a})

instance FromXML LaunchTemplate where
  parseXML x = LaunchTemplate' <$> (x .@? "Id")

instance Hashable LaunchTemplate

instance NFData LaunchTemplate
