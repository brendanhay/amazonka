{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Trigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Trigger where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a trigger.
--
--
--
-- /See:/ 'trigger' smart constructor.
newtype Trigger = Trigger' {_tName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tName' - The name of the trigger.
trigger ::
  Trigger
trigger = Trigger' {_tName = Nothing}

-- | The name of the trigger.
tName :: Lens' Trigger (Maybe Text)
tName = lens _tName (\s a -> s {_tName = a})

instance FromXML Trigger where
  parseXML x = Trigger' <$> (x .@? "Name")

instance Hashable Trigger

instance NFData Trigger
