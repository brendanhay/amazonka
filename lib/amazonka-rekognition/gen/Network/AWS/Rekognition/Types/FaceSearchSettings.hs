{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceSearchSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceSearchSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Input face recognition parameters for an Amazon Rekognition stream processor. @FaceRecognitionSettings@ is a request parameter for 'CreateStreamProcessor' .
--
--
--
-- /See:/ 'faceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { _fssFaceMatchThreshold ::
      !(Maybe Double),
    _fssCollectionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaceSearchSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssFaceMatchThreshold' - Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
--
-- * 'fssCollectionId' - The ID of a collection that contains faces that you want to search for.
faceSearchSettings ::
  FaceSearchSettings
faceSearchSettings =
  FaceSearchSettings'
    { _fssFaceMatchThreshold = Nothing,
      _fssCollectionId = Nothing
    }

-- | Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
fssFaceMatchThreshold :: Lens' FaceSearchSettings (Maybe Double)
fssFaceMatchThreshold = lens _fssFaceMatchThreshold (\s a -> s {_fssFaceMatchThreshold = a})

-- | The ID of a collection that contains faces that you want to search for.
fssCollectionId :: Lens' FaceSearchSettings (Maybe Text)
fssCollectionId = lens _fssCollectionId (\s a -> s {_fssCollectionId = a})

instance FromJSON FaceSearchSettings where
  parseJSON =
    withObject
      "FaceSearchSettings"
      ( \x ->
          FaceSearchSettings'
            <$> (x .:? "FaceMatchThreshold") <*> (x .:? "CollectionId")
      )

instance Hashable FaceSearchSettings

instance NFData FaceSearchSettings

instance ToJSON FaceSearchSettings where
  toJSON FaceSearchSettings' {..} =
    object
      ( catMaybes
          [ ("FaceMatchThreshold" .=) <$> _fssFaceMatchThreshold,
            ("CollectionId" .=) <$> _fssCollectionId
          ]
      )
