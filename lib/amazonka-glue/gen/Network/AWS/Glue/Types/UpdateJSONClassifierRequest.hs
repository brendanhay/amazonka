{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateJSONClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateJSONClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a JSON classifier to be updated.
--
--
--
-- /See:/ 'updateJSONClassifierRequest' smart constructor.
data UpdateJSONClassifierRequest = UpdateJSONClassifierRequest'
  { _ujcrJSONPath ::
      !(Maybe Text),
    _ujcrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujcrJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- * 'ujcrName' - The name of the classifier.
updateJSONClassifierRequest ::
  -- | 'ujcrName'
  Text ->
  UpdateJSONClassifierRequest
updateJSONClassifierRequest pName_ =
  UpdateJSONClassifierRequest'
    { _ujcrJSONPath = Nothing,
      _ujcrName = pName_
    }

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
ujcrJSONPath :: Lens' UpdateJSONClassifierRequest (Maybe Text)
ujcrJSONPath = lens _ujcrJSONPath (\s a -> s {_ujcrJSONPath = a})

-- | The name of the classifier.
ujcrName :: Lens' UpdateJSONClassifierRequest Text
ujcrName = lens _ujcrName (\s a -> s {_ujcrName = a})

instance Hashable UpdateJSONClassifierRequest

instance NFData UpdateJSONClassifierRequest

instance ToJSON UpdateJSONClassifierRequest where
  toJSON UpdateJSONClassifierRequest' {..} =
    object
      ( catMaybes
          [("JsonPath" .=) <$> _ujcrJSONPath, Just ("Name" .= _ujcrName)]
      )
