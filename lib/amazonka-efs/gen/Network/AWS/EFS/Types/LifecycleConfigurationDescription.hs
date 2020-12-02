{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecycleConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecycleConfigurationDescription where

import Network.AWS.EFS.Types.LifecyclePolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'lifecycleConfigurationDescription' smart constructor.
newtype LifecycleConfigurationDescription = LifecycleConfigurationDescription'
  { _lcdLifecyclePolicies ::
      Maybe
        [LifecyclePolicy]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdLifecyclePolicies' - An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
lifecycleConfigurationDescription ::
  LifecycleConfigurationDescription
lifecycleConfigurationDescription =
  LifecycleConfigurationDescription'
    { _lcdLifecyclePolicies =
        Nothing
    }

-- | An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
lcdLifecyclePolicies :: Lens' LifecycleConfigurationDescription [LifecyclePolicy]
lcdLifecyclePolicies = lens _lcdLifecyclePolicies (\s a -> s {_lcdLifecyclePolicies = a}) . _Default . _Coerce

instance FromJSON LifecycleConfigurationDescription where
  parseJSON =
    withObject
      "LifecycleConfigurationDescription"
      ( \x ->
          LifecycleConfigurationDescription'
            <$> (x .:? "LifecyclePolicies" .!= mempty)
      )

instance Hashable LifecycleConfigurationDescription

instance NFData LifecycleConfigurationDescription
