{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a container image that is registered to an Amazon Lightsail container service.
--
--
--
-- /See:/ 'containerImage' smart constructor.
data ContainerImage = ContainerImage'
  { _ciImage :: !(Maybe Text),
    _ciCreatedAt :: !(Maybe POSIX),
    _ciDigest :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciImage' - The name of the container image.
--
-- * 'ciCreatedAt' - The timestamp when the container image was created.
--
-- * 'ciDigest' - The digest of the container image.
containerImage ::
  ContainerImage
containerImage =
  ContainerImage'
    { _ciImage = Nothing,
      _ciCreatedAt = Nothing,
      _ciDigest = Nothing
    }

-- | The name of the container image.
ciImage :: Lens' ContainerImage (Maybe Text)
ciImage = lens _ciImage (\s a -> s {_ciImage = a})

-- | The timestamp when the container image was created.
ciCreatedAt :: Lens' ContainerImage (Maybe UTCTime)
ciCreatedAt = lens _ciCreatedAt (\s a -> s {_ciCreatedAt = a}) . mapping _Time

-- | The digest of the container image.
ciDigest :: Lens' ContainerImage (Maybe Text)
ciDigest = lens _ciDigest (\s a -> s {_ciDigest = a})

instance FromJSON ContainerImage where
  parseJSON =
    withObject
      "ContainerImage"
      ( \x ->
          ContainerImage'
            <$> (x .:? "image") <*> (x .:? "createdAt") <*> (x .:? "digest")
      )

instance Hashable ContainerImage

instance NFData ContainerImage
