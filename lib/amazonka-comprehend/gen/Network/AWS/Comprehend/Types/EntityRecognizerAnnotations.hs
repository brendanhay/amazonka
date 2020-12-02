{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerAnnotations where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the annotations associated with a entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerAnnotations' smart constructor.
newtype EntityRecognizerAnnotations = EntityRecognizerAnnotations'
  { _eraS3URI ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerAnnotations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eraS3URI' - Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
entityRecognizerAnnotations ::
  -- | 'eraS3URI'
  Text ->
  EntityRecognizerAnnotations
entityRecognizerAnnotations pS3URI_ =
  EntityRecognizerAnnotations' {_eraS3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
eraS3URI :: Lens' EntityRecognizerAnnotations Text
eraS3URI = lens _eraS3URI (\s a -> s {_eraS3URI = a})

instance FromJSON EntityRecognizerAnnotations where
  parseJSON =
    withObject
      "EntityRecognizerAnnotations"
      (\x -> EntityRecognizerAnnotations' <$> (x .: "S3Uri"))

instance Hashable EntityRecognizerAnnotations

instance NFData EntityRecognizerAnnotations

instance ToJSON EntityRecognizerAnnotations where
  toJSON EntityRecognizerAnnotations' {..} =
    object (catMaybes [Just ("S3Uri" .= _eraS3URI)])
