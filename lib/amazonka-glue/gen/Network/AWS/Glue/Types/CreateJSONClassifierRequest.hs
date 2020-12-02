{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateJSONClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateJSONClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a JSON classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createJSONClassifierRequest' smart constructor.
data CreateJSONClassifierRequest = CreateJSONClassifierRequest'
  { _cjcrName ::
      !Text,
    _cjcrJSONPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjcrName' - The name of the classifier.
--
-- * 'cjcrJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
createJSONClassifierRequest ::
  -- | 'cjcrName'
  Text ->
  -- | 'cjcrJSONPath'
  Text ->
  CreateJSONClassifierRequest
createJSONClassifierRequest pName_ pJSONPath_ =
  CreateJSONClassifierRequest'
    { _cjcrName = pName_,
      _cjcrJSONPath = pJSONPath_
    }

-- | The name of the classifier.
cjcrName :: Lens' CreateJSONClassifierRequest Text
cjcrName = lens _cjcrName (\s a -> s {_cjcrName = a})

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
cjcrJSONPath :: Lens' CreateJSONClassifierRequest Text
cjcrJSONPath = lens _cjcrJSONPath (\s a -> s {_cjcrJSONPath = a})

instance Hashable CreateJSONClassifierRequest

instance NFData CreateJSONClassifierRequest

instance ToJSON CreateJSONClassifierRequest where
  toJSON CreateJSONClassifierRequest' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _cjcrName), Just ("JsonPath" .= _cjcrJSONPath)]
      )
