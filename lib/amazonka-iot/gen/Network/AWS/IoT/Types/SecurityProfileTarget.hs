{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A target to which an alert is sent when a security profile behavior is violated.
--
--
--
-- /See:/ 'securityProfileTarget' smart constructor.
newtype SecurityProfileTarget = SecurityProfileTarget'
  { _sptArn ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityProfileTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sptArn' - The ARN of the security profile.
securityProfileTarget ::
  -- | 'sptArn'
  Text ->
  SecurityProfileTarget
securityProfileTarget pArn_ =
  SecurityProfileTarget' {_sptArn = pArn_}

-- | The ARN of the security profile.
sptArn :: Lens' SecurityProfileTarget Text
sptArn = lens _sptArn (\s a -> s {_sptArn = a})

instance FromJSON SecurityProfileTarget where
  parseJSON =
    withObject
      "SecurityProfileTarget"
      (\x -> SecurityProfileTarget' <$> (x .: "arn"))

instance Hashable SecurityProfileTarget

instance NFData SecurityProfileTarget
