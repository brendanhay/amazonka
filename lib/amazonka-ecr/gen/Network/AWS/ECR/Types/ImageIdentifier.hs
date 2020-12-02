{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object with identifying information for an Amazon ECR image.
--
--
--
-- /See:/ 'imageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { _iiImageDigest ::
      !(Maybe Text),
    _iiImageTag :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageDigest' - The @sha256@ digest of the image manifest.
--
-- * 'iiImageTag' - The tag used for the image.
imageIdentifier ::
  ImageIdentifier
imageIdentifier =
  ImageIdentifier' {_iiImageDigest = Nothing, _iiImageTag = Nothing}

-- | The @sha256@ digest of the image manifest.
iiImageDigest :: Lens' ImageIdentifier (Maybe Text)
iiImageDigest = lens _iiImageDigest (\s a -> s {_iiImageDigest = a})

-- | The tag used for the image.
iiImageTag :: Lens' ImageIdentifier (Maybe Text)
iiImageTag = lens _iiImageTag (\s a -> s {_iiImageTag = a})

instance FromJSON ImageIdentifier where
  parseJSON =
    withObject
      "ImageIdentifier"
      ( \x ->
          ImageIdentifier' <$> (x .:? "imageDigest") <*> (x .:? "imageTag")
      )

instance Hashable ImageIdentifier

instance NFData ImageIdentifier

instance ToJSON ImageIdentifier where
  toJSON ImageIdentifier' {..} =
    object
      ( catMaybes
          [ ("imageDigest" .=) <$> _iiImageDigest,
            ("imageTag" .=) <$> _iiImageTag
          ]
      )
