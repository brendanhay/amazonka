{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CompareFacesMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CompareFacesMatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a face in a target image that matches the source image face analyzed by @CompareFaces@ . The @Face@ property contains the bounding box of the face in the target image. The @Similarity@ property is the confidence that the source image face matches the face in the bounding box.
--
--
--
-- /See:/ 'compareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { _cfmSimilarity ::
      !(Maybe Double),
    _cfmFace :: !(Maybe ComparedFace)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompareFacesMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfmSimilarity' - Level of confidence that the faces match.
--
-- * 'cfmFace' - Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
compareFacesMatch ::
  CompareFacesMatch
compareFacesMatch =
  CompareFacesMatch' {_cfmSimilarity = Nothing, _cfmFace = Nothing}

-- | Level of confidence that the faces match.
cfmSimilarity :: Lens' CompareFacesMatch (Maybe Double)
cfmSimilarity = lens _cfmSimilarity (\s a -> s {_cfmSimilarity = a})

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
cfmFace :: Lens' CompareFacesMatch (Maybe ComparedFace)
cfmFace = lens _cfmFace (\s a -> s {_cfmFace = a})

instance FromJSON CompareFacesMatch where
  parseJSON =
    withObject
      "CompareFacesMatch"
      ( \x ->
          CompareFacesMatch' <$> (x .:? "Similarity") <*> (x .:? "Face")
      )

instance Hashable CompareFacesMatch

instance NFData CompareFacesMatch
