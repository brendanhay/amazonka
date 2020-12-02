{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetection where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.PersonDetail

-- | Details and path tracking information for a single time a person's path is tracked in a video. Amazon Rekognition operations that track people's paths return an array of @PersonDetection@ objects with elements for each time a person's path is tracked in a video.
--
--
-- For more information, see GetPersonTracking in the Amazon Rekognition Developer Guide.
--
--
-- /See:/ 'personDetection' smart constructor.
data PersonDetection = PersonDetection'
  { _pdPerson ::
      !(Maybe PersonDetail),
    _pdTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PersonDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPerson' - Details about a person whose path was tracked in a video.
--
-- * 'pdTimestamp' - The time, in milliseconds from the start of the video, that the person's path was tracked.
personDetection ::
  PersonDetection
personDetection =
  PersonDetection' {_pdPerson = Nothing, _pdTimestamp = Nothing}

-- | Details about a person whose path was tracked in a video.
pdPerson :: Lens' PersonDetection (Maybe PersonDetail)
pdPerson = lens _pdPerson (\s a -> s {_pdPerson = a})

-- | The time, in milliseconds from the start of the video, that the person's path was tracked.
pdTimestamp :: Lens' PersonDetection (Maybe Integer)
pdTimestamp = lens _pdTimestamp (\s a -> s {_pdTimestamp = a})

instance FromJSON PersonDetection where
  parseJSON =
    withObject
      "PersonDetection"
      ( \x ->
          PersonDetection' <$> (x .:? "Person") <*> (x .:? "Timestamp")
      )

instance Hashable PersonDetection

instance NFData PersonDetection
