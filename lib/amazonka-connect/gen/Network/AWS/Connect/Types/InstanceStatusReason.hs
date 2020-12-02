{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatusReason where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Relevant details why the instance was not successfully created.
--
--
--
-- /See:/ 'instanceStatusReason' smart constructor.
newtype InstanceStatusReason = InstanceStatusReason'
  { _isrMessage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatusReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrMessage' - The message.
instanceStatusReason ::
  InstanceStatusReason
instanceStatusReason = InstanceStatusReason' {_isrMessage = Nothing}

-- | The message.
isrMessage :: Lens' InstanceStatusReason (Maybe Text)
isrMessage = lens _isrMessage (\s a -> s {_isrMessage = a})

instance FromJSON InstanceStatusReason where
  parseJSON =
    withObject
      "InstanceStatusReason"
      (\x -> InstanceStatusReason' <$> (x .:? "Message"))

instance Hashable InstanceStatusReason

instance NFData InstanceStatusReason
