{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.FaceDetail

-- | Details about a person detected in a video analysis request.
--
--
--
-- /See:/ 'personDetail' smart constructor.
data PersonDetail = PersonDetail'
  { _pdBoundingBox ::
      !(Maybe BoundingBox),
    _pdIndex :: !(Maybe Integer),
    _pdFace :: !(Maybe FaceDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PersonDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdBoundingBox' - Bounding box around the detected person.
--
-- * 'pdIndex' - Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
--
-- * 'pdFace' - Face details for the detected person.
personDetail ::
  PersonDetail
personDetail =
  PersonDetail'
    { _pdBoundingBox = Nothing,
      _pdIndex = Nothing,
      _pdFace = Nothing
    }

-- | Bounding box around the detected person.
pdBoundingBox :: Lens' PersonDetail (Maybe BoundingBox)
pdBoundingBox = lens _pdBoundingBox (\s a -> s {_pdBoundingBox = a})

-- | Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
pdIndex :: Lens' PersonDetail (Maybe Integer)
pdIndex = lens _pdIndex (\s a -> s {_pdIndex = a})

-- | Face details for the detected person.
pdFace :: Lens' PersonDetail (Maybe FaceDetail)
pdFace = lens _pdFace (\s a -> s {_pdFace = a})

instance FromJSON PersonDetail where
  parseJSON =
    withObject
      "PersonDetail"
      ( \x ->
          PersonDetail'
            <$> (x .:? "BoundingBox") <*> (x .:? "Index") <*> (x .:? "Face")
      )

instance Hashable PersonDetail

instance NFData PersonDetail
