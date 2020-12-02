{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.FaceDetail

-- | Information about a recognized celebrity.
--
--
--
-- /See:/ 'celebrityDetail' smart constructor.
data CelebrityDetail = CelebrityDetail'
  { _cdBoundingBox ::
      !(Maybe BoundingBox),
    _cdURLs :: !(Maybe [Text]),
    _cdConfidence :: !(Maybe Double),
    _cdName :: !(Maybe Text),
    _cdId :: !(Maybe Text),
    _cdFace :: !(Maybe FaceDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CelebrityDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdBoundingBox' - Bounding box around the body of a celebrity.
--
-- * 'cdURLs' - An array of URLs pointing to additional celebrity information.
--
-- * 'cdConfidence' - The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- * 'cdName' - The name of the celebrity.
--
-- * 'cdId' - The unique identifier for the celebrity.
--
-- * 'cdFace' - Face details for the recognized celebrity.
celebrityDetail ::
  CelebrityDetail
celebrityDetail =
  CelebrityDetail'
    { _cdBoundingBox = Nothing,
      _cdURLs = Nothing,
      _cdConfidence = Nothing,
      _cdName = Nothing,
      _cdId = Nothing,
      _cdFace = Nothing
    }

-- | Bounding box around the body of a celebrity.
cdBoundingBox :: Lens' CelebrityDetail (Maybe BoundingBox)
cdBoundingBox = lens _cdBoundingBox (\s a -> s {_cdBoundingBox = a})

-- | An array of URLs pointing to additional celebrity information.
cdURLs :: Lens' CelebrityDetail [Text]
cdURLs = lens _cdURLs (\s a -> s {_cdURLs = a}) . _Default . _Coerce

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
cdConfidence :: Lens' CelebrityDetail (Maybe Double)
cdConfidence = lens _cdConfidence (\s a -> s {_cdConfidence = a})

-- | The name of the celebrity.
cdName :: Lens' CelebrityDetail (Maybe Text)
cdName = lens _cdName (\s a -> s {_cdName = a})

-- | The unique identifier for the celebrity.
cdId :: Lens' CelebrityDetail (Maybe Text)
cdId = lens _cdId (\s a -> s {_cdId = a})

-- | Face details for the recognized celebrity.
cdFace :: Lens' CelebrityDetail (Maybe FaceDetail)
cdFace = lens _cdFace (\s a -> s {_cdFace = a})

instance FromJSON CelebrityDetail where
  parseJSON =
    withObject
      "CelebrityDetail"
      ( \x ->
          CelebrityDetail'
            <$> (x .:? "BoundingBox")
            <*> (x .:? "Urls" .!= mempty)
            <*> (x .:? "Confidence")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Face")
      )

instance Hashable CelebrityDetail

instance NFData CelebrityDetail
