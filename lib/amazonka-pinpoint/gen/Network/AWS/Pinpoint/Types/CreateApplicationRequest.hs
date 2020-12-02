{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateApplicationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateApplicationRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the display name of an application and the tags to associate with the application.
--
--
--
-- /See:/ 'createApplicationRequest' smart constructor.
data CreateApplicationRequest = CreateApplicationRequest'
  { _carTags ::
      !(Maybe (Map Text (Text))),
    _carName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateApplicationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carTags' - A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
--
-- * 'carName' - The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
createApplicationRequest ::
  -- | 'carName'
  Text ->
  CreateApplicationRequest
createApplicationRequest pName_ =
  CreateApplicationRequest' {_carTags = Nothing, _carName = pName_}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
carTags :: Lens' CreateApplicationRequest (HashMap Text (Text))
carTags = lens _carTags (\s a -> s {_carTags = a}) . _Default . _Map

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
carName :: Lens' CreateApplicationRequest Text
carName = lens _carName (\s a -> s {_carName = a})

instance Hashable CreateApplicationRequest

instance NFData CreateApplicationRequest

instance ToJSON CreateApplicationRequest where
  toJSON CreateApplicationRequest' {..} =
    object
      (catMaybes [("tags" .=) <$> _carTags, Just ("Name" .= _carName)])
