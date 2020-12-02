{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroup where

import Network.AWS.CloudFront.Types.KeyGroupConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key group.
--
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
--
-- /See:/ 'keyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { _kgId :: !Text,
    _kgLastModifiedTime :: !ISO8601,
    _kgKeyGroupConfig :: !KeyGroupConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgId' - The identifier for the key group.
--
-- * 'kgLastModifiedTime' - The date and time when the key group was last modified.
--
-- * 'kgKeyGroupConfig' - The key group configuration.
keyGroup ::
  -- | 'kgId'
  Text ->
  -- | 'kgLastModifiedTime'
  UTCTime ->
  -- | 'kgKeyGroupConfig'
  KeyGroupConfig ->
  KeyGroup
keyGroup pId_ pLastModifiedTime_ pKeyGroupConfig_ =
  KeyGroup'
    { _kgId = pId_,
      _kgLastModifiedTime = _Time # pLastModifiedTime_,
      _kgKeyGroupConfig = pKeyGroupConfig_
    }

-- | The identifier for the key group.
kgId :: Lens' KeyGroup Text
kgId = lens _kgId (\s a -> s {_kgId = a})

-- | The date and time when the key group was last modified.
kgLastModifiedTime :: Lens' KeyGroup UTCTime
kgLastModifiedTime = lens _kgLastModifiedTime (\s a -> s {_kgLastModifiedTime = a}) . _Time

-- | The key group configuration.
kgKeyGroupConfig :: Lens' KeyGroup KeyGroupConfig
kgKeyGroupConfig = lens _kgKeyGroupConfig (\s a -> s {_kgKeyGroupConfig = a})

instance FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      <$> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "KeyGroupConfig")

instance Hashable KeyGroup

instance NFData KeyGroup
