{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JSONClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JSONClassifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A classifier for @JSON@ content.
--
--
--
-- /See:/ 'jsonClassifier' smart constructor.
data JSONClassifier = JSONClassifier'
  { _jcCreationTime ::
      !(Maybe POSIX),
    _jcLastUpdated :: !(Maybe POSIX),
    _jcVersion :: !(Maybe Integer),
    _jcName :: !Text,
    _jcJSONPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JSONClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jcCreationTime' - The time that this classifier was registered.
--
-- * 'jcLastUpdated' - The time that this classifier was last updated.
--
-- * 'jcVersion' - The version of this classifier.
--
-- * 'jcName' - The name of the classifier.
--
-- * 'jcJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
jsonClassifier ::
  -- | 'jcName'
  Text ->
  -- | 'jcJSONPath'
  Text ->
  JSONClassifier
jsonClassifier pName_ pJSONPath_ =
  JSONClassifier'
    { _jcCreationTime = Nothing,
      _jcLastUpdated = Nothing,
      _jcVersion = Nothing,
      _jcName = pName_,
      _jcJSONPath = pJSONPath_
    }

-- | The time that this classifier was registered.
jcCreationTime :: Lens' JSONClassifier (Maybe UTCTime)
jcCreationTime = lens _jcCreationTime (\s a -> s {_jcCreationTime = a}) . mapping _Time

-- | The time that this classifier was last updated.
jcLastUpdated :: Lens' JSONClassifier (Maybe UTCTime)
jcLastUpdated = lens _jcLastUpdated (\s a -> s {_jcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
jcVersion :: Lens' JSONClassifier (Maybe Integer)
jcVersion = lens _jcVersion (\s a -> s {_jcVersion = a})

-- | The name of the classifier.
jcName :: Lens' JSONClassifier Text
jcName = lens _jcName (\s a -> s {_jcName = a})

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
jcJSONPath :: Lens' JSONClassifier Text
jcJSONPath = lens _jcJSONPath (\s a -> s {_jcJSONPath = a})

instance FromJSON JSONClassifier where
  parseJSON =
    withObject
      "JSONClassifier"
      ( \x ->
          JSONClassifier'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "Version")
            <*> (x .: "Name")
            <*> (x .: "JsonPath")
      )

instance Hashable JSONClassifier

instance NFData JSONClassifier
