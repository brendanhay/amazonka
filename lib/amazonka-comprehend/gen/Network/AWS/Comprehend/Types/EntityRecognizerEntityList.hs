{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEntityList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEntityList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the entity recognizer submitted with an entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerEntityList' smart constructor.
newtype EntityRecognizerEntityList = EntityRecognizerEntityList'
  { _erelS3URI ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerEntityList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erelS3URI' - Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
entityRecognizerEntityList ::
  -- | 'erelS3URI'
  Text ->
  EntityRecognizerEntityList
entityRecognizerEntityList pS3URI_ =
  EntityRecognizerEntityList' {_erelS3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
erelS3URI :: Lens' EntityRecognizerEntityList Text
erelS3URI = lens _erelS3URI (\s a -> s {_erelS3URI = a})

instance FromJSON EntityRecognizerEntityList where
  parseJSON =
    withObject
      "EntityRecognizerEntityList"
      (\x -> EntityRecognizerEntityList' <$> (x .: "S3Uri"))

instance Hashable EntityRecognizerEntityList

instance NFData EntityRecognizerEntityList

instance ToJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList' {..} =
    object (catMaybes [Just ("S3Uri" .= _erelS3URI)])
