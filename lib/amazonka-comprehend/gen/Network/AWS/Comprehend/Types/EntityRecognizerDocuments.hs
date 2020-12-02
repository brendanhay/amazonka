{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerDocuments where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the training documents submitted with an entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerDocuments' smart constructor.
newtype EntityRecognizerDocuments = EntityRecognizerDocuments'
  { _erdS3URI ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdS3URI' - Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
entityRecognizerDocuments ::
  -- | 'erdS3URI'
  Text ->
  EntityRecognizerDocuments
entityRecognizerDocuments pS3URI_ =
  EntityRecognizerDocuments' {_erdS3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
erdS3URI :: Lens' EntityRecognizerDocuments Text
erdS3URI = lens _erdS3URI (\s a -> s {_erdS3URI = a})

instance FromJSON EntityRecognizerDocuments where
  parseJSON =
    withObject
      "EntityRecognizerDocuments"
      (\x -> EntityRecognizerDocuments' <$> (x .: "S3Uri"))

instance Hashable EntityRecognizerDocuments

instance NFData EntityRecognizerDocuments

instance ToJSON EntityRecognizerDocuments where
  toJSON EntityRecognizerDocuments' {..} =
    object (catMaybes [Just ("S3Uri" .= _erdS3URI)])
