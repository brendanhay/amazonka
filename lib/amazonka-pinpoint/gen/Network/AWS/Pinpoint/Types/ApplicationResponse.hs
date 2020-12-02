{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about an application.
--
--
--
-- /See:/ 'applicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { _appTags ::
      !(Maybe (Map Text (Text))),
    _appId :: !Text,
    _appARN :: !Text,
    _appName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
--
-- * 'appId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'appARN' - The Amazon Resource Name (ARN) of the application.
--
-- * 'appName' - The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
applicationResponse ::
  -- | 'appId'
  Text ->
  -- | 'appARN'
  Text ->
  -- | 'appName'
  Text ->
  ApplicationResponse
applicationResponse pId_ pARN_ pName_ =
  ApplicationResponse'
    { _appTags = Nothing,
      _appId = pId_,
      _appARN = pARN_,
      _appName = pName_
    }

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
appTags :: Lens' ApplicationResponse (HashMap Text (Text))
appTags = lens _appTags (\s a -> s {_appTags = a}) . _Default . _Map

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
appId :: Lens' ApplicationResponse Text
appId = lens _appId (\s a -> s {_appId = a})

-- | The Amazon Resource Name (ARN) of the application.
appARN :: Lens' ApplicationResponse Text
appARN = lens _appARN (\s a -> s {_appARN = a})

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
appName :: Lens' ApplicationResponse Text
appName = lens _appName (\s a -> s {_appName = a})

instance FromJSON ApplicationResponse where
  parseJSON =
    withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            <$> (x .:? "tags" .!= mempty)
            <*> (x .: "Id")
            <*> (x .: "Arn")
            <*> (x .: "Name")
      )

instance Hashable ApplicationResponse

instance NFData ApplicationResponse
