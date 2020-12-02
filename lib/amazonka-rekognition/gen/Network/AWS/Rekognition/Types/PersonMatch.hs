{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonMatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.PersonDetail

-- | Information about a person whose face matches a face(s) in an Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection ('FaceMatch' ), information about the person ('PersonDetail' ), and the time stamp for when the person was detected in a video. An array of @PersonMatch@ objects is returned by 'GetFaceSearch' .
--
--
--
-- /See:/ 'personMatch' smart constructor.
data PersonMatch = PersonMatch'
  { _pmFaceMatches ::
      !(Maybe [FaceMatch]),
    _pmPerson :: !(Maybe PersonDetail),
    _pmTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PersonMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmFaceMatches' - Information about the faces in the input collection that match the face of a person in the video.
--
-- * 'pmPerson' - Information about the matched person.
--
-- * 'pmTimestamp' - The time, in milliseconds from the beginning of the video, that the person was matched in the video.
personMatch ::
  PersonMatch
personMatch =
  PersonMatch'
    { _pmFaceMatches = Nothing,
      _pmPerson = Nothing,
      _pmTimestamp = Nothing
    }

-- | Information about the faces in the input collection that match the face of a person in the video.
pmFaceMatches :: Lens' PersonMatch [FaceMatch]
pmFaceMatches = lens _pmFaceMatches (\s a -> s {_pmFaceMatches = a}) . _Default . _Coerce

-- | Information about the matched person.
pmPerson :: Lens' PersonMatch (Maybe PersonDetail)
pmPerson = lens _pmPerson (\s a -> s {_pmPerson = a})

-- | The time, in milliseconds from the beginning of the video, that the person was matched in the video.
pmTimestamp :: Lens' PersonMatch (Maybe Integer)
pmTimestamp = lens _pmTimestamp (\s a -> s {_pmTimestamp = a})

instance FromJSON PersonMatch where
  parseJSON =
    withObject
      "PersonMatch"
      ( \x ->
          PersonMatch'
            <$> (x .:? "FaceMatches" .!= mempty)
            <*> (x .:? "Person")
            <*> (x .:? "Timestamp")
      )

instance Hashable PersonMatch

instance NFData PersonMatch
