{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionLimits where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.Limit

-- | Limits settings on protections for your subscription.
--
--
--
-- /See:/ 'protectionLimits' smart constructor.
newtype ProtectionLimits = ProtectionLimits'
  { _plProtectedResourceTypeLimits ::
      [Limit]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectionLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plProtectedResourceTypeLimits' - The maximum number of resource types that you can specify in a protection.
protectionLimits ::
  ProtectionLimits
protectionLimits =
  ProtectionLimits' {_plProtectedResourceTypeLimits = mempty}

-- | The maximum number of resource types that you can specify in a protection.
plProtectedResourceTypeLimits :: Lens' ProtectionLimits [Limit]
plProtectedResourceTypeLimits = lens _plProtectedResourceTypeLimits (\s a -> s {_plProtectedResourceTypeLimits = a}) . _Coerce

instance FromJSON ProtectionLimits where
  parseJSON =
    withObject
      "ProtectionLimits"
      ( \x ->
          ProtectionLimits'
            <$> (x .:? "ProtectedResourceTypeLimits" .!= mempty)
      )

instance Hashable ProtectionLimits

instance NFData ProtectionLimits
