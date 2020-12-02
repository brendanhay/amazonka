{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.UnindexedFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.UnindexedFace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.FaceDetail
import Network.AWS.Rekognition.Types.Reason

-- | A face that 'IndexFaces' detected, but didn't index. Use the @Reasons@ response attribute to determine why a face wasn't indexed.
--
--
--
-- /See:/ 'unindexedFace' smart constructor.
data UnindexedFace = UnindexedFace'
  { _ufReasons ::
      !(Maybe [Reason]),
    _ufFaceDetail :: !(Maybe FaceDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnindexedFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufReasons' - An array of reasons that specify why a face wasn't indexed.      * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .     * LOW_BRIGHTNESS - The image is too dark.     * LOW_SHARPNESS - The image is too blurry.     * LOW_CONFIDENCE - The face was detected with a low confidence.     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
--
-- * 'ufFaceDetail' - The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
unindexedFace ::
  UnindexedFace
unindexedFace =
  UnindexedFace' {_ufReasons = Nothing, _ufFaceDetail = Nothing}

-- | An array of reasons that specify why a face wasn't indexed.      * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .     * LOW_BRIGHTNESS - The image is too dark.     * LOW_SHARPNESS - The image is too blurry.     * LOW_CONFIDENCE - The face was detected with a low confidence.     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
ufReasons :: Lens' UnindexedFace [Reason]
ufReasons = lens _ufReasons (\s a -> s {_ufReasons = a}) . _Default . _Coerce

-- | The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
ufFaceDetail :: Lens' UnindexedFace (Maybe FaceDetail)
ufFaceDetail = lens _ufFaceDetail (\s a -> s {_ufFaceDetail = a})

instance FromJSON UnindexedFace where
  parseJSON =
    withObject
      "UnindexedFace"
      ( \x ->
          UnindexedFace'
            <$> (x .:? "Reasons" .!= mempty) <*> (x .:? "FaceDetail")
      )

instance Hashable UnindexedFace

instance NFData UnindexedFace
