{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameSuggestion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameSuggestion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A property name returned from a @GetSearchSuggestions@ call that specifies a value in the @PropertyNameQuery@ field.
--
--
--
-- /See:/ 'propertyNameSuggestion' smart constructor.
newtype PropertyNameSuggestion = PropertyNameSuggestion'
  { _pnsPropertyName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PropertyNameSuggestion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnsPropertyName' - A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
propertyNameSuggestion ::
  PropertyNameSuggestion
propertyNameSuggestion =
  PropertyNameSuggestion' {_pnsPropertyName = Nothing}

-- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
pnsPropertyName :: Lens' PropertyNameSuggestion (Maybe Text)
pnsPropertyName = lens _pnsPropertyName (\s a -> s {_pnsPropertyName = a})

instance FromJSON PropertyNameSuggestion where
  parseJSON =
    withObject
      "PropertyNameSuggestion"
      (\x -> PropertyNameSuggestion' <$> (x .:? "PropertyName"))

instance Hashable PropertyNameSuggestion

instance NFData PropertyNameSuggestion
