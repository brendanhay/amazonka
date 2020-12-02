{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.SDKType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SDKType where

import Network.AWS.APIGateway.Types.SDKConfigurationProperty
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A type of SDK that API Gateway can generate.
--
--
--
-- /See:/ 'sdkType' smart constructor.
data SDKType = SDKType'
  { _stFriendlyName :: !(Maybe Text),
    _stConfigurationProperties :: !(Maybe [SDKConfigurationProperty]),
    _stId :: !(Maybe Text),
    _stDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SDKType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stFriendlyName' - The user-friendly name of an 'SdkType' instance.
--
-- * 'stConfigurationProperties' - A list of configuration properties of an 'SdkType' .
--
-- * 'stId' - The identifier of an 'SdkType' instance.
--
-- * 'stDescription' - The description of an 'SdkType' .
sdkType ::
  SDKType
sdkType =
  SDKType'
    { _stFriendlyName = Nothing,
      _stConfigurationProperties = Nothing,
      _stId = Nothing,
      _stDescription = Nothing
    }

-- | The user-friendly name of an 'SdkType' instance.
stFriendlyName :: Lens' SDKType (Maybe Text)
stFriendlyName = lens _stFriendlyName (\s a -> s {_stFriendlyName = a})

-- | A list of configuration properties of an 'SdkType' .
stConfigurationProperties :: Lens' SDKType [SDKConfigurationProperty]
stConfigurationProperties = lens _stConfigurationProperties (\s a -> s {_stConfigurationProperties = a}) . _Default . _Coerce

-- | The identifier of an 'SdkType' instance.
stId :: Lens' SDKType (Maybe Text)
stId = lens _stId (\s a -> s {_stId = a})

-- | The description of an 'SdkType' .
stDescription :: Lens' SDKType (Maybe Text)
stDescription = lens _stDescription (\s a -> s {_stDescription = a})

instance FromJSON SDKType where
  parseJSON =
    withObject
      "SDKType"
      ( \x ->
          SDKType'
            <$> (x .:? "friendlyName")
            <*> (x .:? "configurationProperties" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "description")
      )

instance Hashable SDKType

instance NFData SDKType
