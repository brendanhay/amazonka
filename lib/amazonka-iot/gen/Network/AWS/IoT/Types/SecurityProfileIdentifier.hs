{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifying information for a Device Defender security profile.
--
--
--
-- /See:/ 'securityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { _spiName ::
      !Text,
    _spiArn :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityProfileIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spiName' - The name you have given to the security profile.
--
-- * 'spiArn' - The ARN of the security profile.
securityProfileIdentifier ::
  -- | 'spiName'
  Text ->
  -- | 'spiArn'
  Text ->
  SecurityProfileIdentifier
securityProfileIdentifier pName_ pArn_ =
  SecurityProfileIdentifier' {_spiName = pName_, _spiArn = pArn_}

-- | The name you have given to the security profile.
spiName :: Lens' SecurityProfileIdentifier Text
spiName = lens _spiName (\s a -> s {_spiName = a})

-- | The ARN of the security profile.
spiArn :: Lens' SecurityProfileIdentifier Text
spiArn = lens _spiArn (\s a -> s {_spiArn = a})

instance FromJSON SecurityProfileIdentifier where
  parseJSON =
    withObject
      "SecurityProfileIdentifier"
      ( \x ->
          SecurityProfileIdentifier' <$> (x .: "name") <*> (x .: "arn")
      )

instance Hashable SecurityProfileIdentifier

instance NFData SecurityProfileIdentifier
