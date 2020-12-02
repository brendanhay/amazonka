{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointUser where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies data for one or more attributes that describe the user who's associated with an endpoint.
--
--
--
-- /See:/ 'endpointUser' smart constructor.
data EndpointUser = EndpointUser'
  { _euUserAttributes ::
      !(Maybe (Map Text ([Text]))),
    _euUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'euUserAttributes' - One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- * 'euUserId' - The unique identifier for the user.
endpointUser ::
  EndpointUser
endpointUser =
  EndpointUser' {_euUserAttributes = Nothing, _euUserId = Nothing}

-- | One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive. An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
euUserAttributes :: Lens' EndpointUser (HashMap Text ([Text]))
euUserAttributes = lens _euUserAttributes (\s a -> s {_euUserAttributes = a}) . _Default . _Map

-- | The unique identifier for the user.
euUserId :: Lens' EndpointUser (Maybe Text)
euUserId = lens _euUserId (\s a -> s {_euUserId = a})

instance FromJSON EndpointUser where
  parseJSON =
    withObject
      "EndpointUser"
      ( \x ->
          EndpointUser'
            <$> (x .:? "UserAttributes" .!= mempty) <*> (x .:? "UserId")
      )

instance Hashable EndpointUser

instance NFData EndpointUser

instance ToJSON EndpointUser where
  toJSON EndpointUser' {..} =
    object
      ( catMaybes
          [ ("UserAttributes" .=) <$> _euUserAttributes,
            ("UserId" .=) <$> _euUserId
          ]
      )
