{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfigurationProperty where

import Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an action configuration property.
--
--
--
-- /See:/ 'actionConfigurationProperty' smart constructor.
data ActionConfigurationProperty = ActionConfigurationProperty'
  { _acpQueryable ::
      !(Maybe Bool),
    _acpType ::
      !( Maybe
           ActionConfigurationPropertyType
       ),
    _acpDescription :: !(Maybe Text),
    _acpName :: !Text,
    _acpRequired :: !Bool,
    _acpKey :: !Bool,
    _acpSecret :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionConfigurationProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acpQueryable' - Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret. If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
--
-- * 'acpType' - The type of the configuration property.
--
-- * 'acpDescription' - The description of the action configuration property that is displayed to users.
--
-- * 'acpName' - The name of the action configuration property.
--
-- * 'acpRequired' - Whether the configuration property is a required value.
--
-- * 'acpKey' - Whether the configuration property is a key.
--
-- * 'acpSecret' - Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ . When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
actionConfigurationProperty ::
  -- | 'acpName'
  Text ->
  -- | 'acpRequired'
  Bool ->
  -- | 'acpKey'
  Bool ->
  -- | 'acpSecret'
  Bool ->
  ActionConfigurationProperty
actionConfigurationProperty pName_ pRequired_ pKey_ pSecret_ =
  ActionConfigurationProperty'
    { _acpQueryable = Nothing,
      _acpType = Nothing,
      _acpDescription = Nothing,
      _acpName = pName_,
      _acpRequired = pRequired_,
      _acpKey = pKey_,
      _acpSecret = pSecret_
    }

-- | Indicates that the property is used with @PollForJobs@ . When creating a custom action, an action can have up to one queryable property. If it has one, that property must be both required and not secret. If you create a pipeline with a custom action type, and that custom action contains a queryable property, the value for that configuration property is subject to other restrictions. The value must be less than or equal to twenty (20) characters. The value can contain only alphanumeric characters, underscores, and hyphens.
acpQueryable :: Lens' ActionConfigurationProperty (Maybe Bool)
acpQueryable = lens _acpQueryable (\s a -> s {_acpQueryable = a})

-- | The type of the configuration property.
acpType :: Lens' ActionConfigurationProperty (Maybe ActionConfigurationPropertyType)
acpType = lens _acpType (\s a -> s {_acpType = a})

-- | The description of the action configuration property that is displayed to users.
acpDescription :: Lens' ActionConfigurationProperty (Maybe Text)
acpDescription = lens _acpDescription (\s a -> s {_acpDescription = a})

-- | The name of the action configuration property.
acpName :: Lens' ActionConfigurationProperty Text
acpName = lens _acpName (\s a -> s {_acpName = a})

-- | Whether the configuration property is a required value.
acpRequired :: Lens' ActionConfigurationProperty Bool
acpRequired = lens _acpRequired (\s a -> s {_acpRequired = a})

-- | Whether the configuration property is a key.
acpKey :: Lens' ActionConfigurationProperty Bool
acpKey = lens _acpKey (\s a -> s {_acpKey = a})

-- | Whether the configuration property is secret. Secrets are hidden from all calls except for @GetJobDetails@ , @GetThirdPartyJobDetails@ , @PollForJobs@ , and @PollForThirdPartyJobs@ . When updating a pipeline, passing * * * * * without changing any other values of the action preserves the previous value of the secret.
acpSecret :: Lens' ActionConfigurationProperty Bool
acpSecret = lens _acpSecret (\s a -> s {_acpSecret = a})

instance FromJSON ActionConfigurationProperty where
  parseJSON =
    withObject
      "ActionConfigurationProperty"
      ( \x ->
          ActionConfigurationProperty'
            <$> (x .:? "queryable")
            <*> (x .:? "type")
            <*> (x .:? "description")
            <*> (x .: "name")
            <*> (x .: "required")
            <*> (x .: "key")
            <*> (x .: "secret")
      )

instance Hashable ActionConfigurationProperty

instance NFData ActionConfigurationProperty

instance ToJSON ActionConfigurationProperty where
  toJSON ActionConfigurationProperty' {..} =
    object
      ( catMaybes
          [ ("queryable" .=) <$> _acpQueryable,
            ("type" .=) <$> _acpType,
            ("description" .=) <$> _acpDescription,
            Just ("name" .= _acpName),
            Just ("required" .= _acpRequired),
            Just ("key" .= _acpKey),
            Just ("secret" .= _acpSecret)
          ]
      )
