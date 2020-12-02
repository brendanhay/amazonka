{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecyclePolicy where

import Network.AWS.EFS.Types.TransitionToIARules
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a policy used by EFS lifecycle management to transition files to the Infrequent Access (IA) storage class.
--
--
--
-- /See:/ 'lifecyclePolicy' smart constructor.
newtype LifecyclePolicy = LifecyclePolicy'
  { _lpTransitionToIA ::
      Maybe TransitionToIARules
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpTransitionToIA' - A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
lifecyclePolicy ::
  LifecyclePolicy
lifecyclePolicy = LifecyclePolicy' {_lpTransitionToIA = Nothing}

-- | A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
lpTransitionToIA :: Lens' LifecyclePolicy (Maybe TransitionToIARules)
lpTransitionToIA = lens _lpTransitionToIA (\s a -> s {_lpTransitionToIA = a})

instance FromJSON LifecyclePolicy where
  parseJSON =
    withObject
      "LifecyclePolicy"
      (\x -> LifecyclePolicy' <$> (x .:? "TransitionToIA"))

instance Hashable LifecyclePolicy

instance NFData LifecyclePolicy

instance ToJSON LifecyclePolicy where
  toJSON LifecyclePolicy' {..} =
    object (catMaybes [("TransitionToIA" .=) <$> _lpTransitionToIA])
