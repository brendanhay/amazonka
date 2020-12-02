{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImageInserter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImageInserter where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.InsertableImage
import Network.AWS.Prelude

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input or output individually. This setting is disabled by default.
--
-- /See:/ 'imageInserter' smart constructor.
newtype ImageInserter = ImageInserter'
  { _iiInsertableImages ::
      Maybe [InsertableImage]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageInserter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInsertableImages' - Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
imageInserter ::
  ImageInserter
imageInserter = ImageInserter' {_iiInsertableImages = Nothing}

-- | Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
iiInsertableImages :: Lens' ImageInserter [InsertableImage]
iiInsertableImages = lens _iiInsertableImages (\s a -> s {_iiInsertableImages = a}) . _Default . _Coerce

instance FromJSON ImageInserter where
  parseJSON =
    withObject
      "ImageInserter"
      (\x -> ImageInserter' <$> (x .:? "insertableImages" .!= mempty))

instance Hashable ImageInserter

instance NFData ImageInserter

instance ToJSON ImageInserter where
  toJSON ImageInserter' {..} =
    object
      (catMaybes [("insertableImages" .=) <$> _iiInsertableImages])
