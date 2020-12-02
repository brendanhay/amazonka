{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupValidationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ServerValidationConfiguration

-- | Configuration for validating an instance.
--
--
--
-- /See:/ 'serverGroupValidationConfiguration' smart constructor.
data ServerGroupValidationConfiguration = ServerGroupValidationConfiguration'
  { _sgvcServerValidationConfigurations ::
      !( Maybe
           [ServerValidationConfiguration]
       ),
    _sgvcServerGroupId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerGroupValidationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgvcServerValidationConfigurations' - The validation configuration.
--
-- * 'sgvcServerGroupId' - The ID of the server group.
serverGroupValidationConfiguration ::
  ServerGroupValidationConfiguration
serverGroupValidationConfiguration =
  ServerGroupValidationConfiguration'
    { _sgvcServerValidationConfigurations =
        Nothing,
      _sgvcServerGroupId = Nothing
    }

-- | The validation configuration.
sgvcServerValidationConfigurations :: Lens' ServerGroupValidationConfiguration [ServerValidationConfiguration]
sgvcServerValidationConfigurations = lens _sgvcServerValidationConfigurations (\s a -> s {_sgvcServerValidationConfigurations = a}) . _Default . _Coerce

-- | The ID of the server group.
sgvcServerGroupId :: Lens' ServerGroupValidationConfiguration (Maybe Text)
sgvcServerGroupId = lens _sgvcServerGroupId (\s a -> s {_sgvcServerGroupId = a})

instance FromJSON ServerGroupValidationConfiguration where
  parseJSON =
    withObject
      "ServerGroupValidationConfiguration"
      ( \x ->
          ServerGroupValidationConfiguration'
            <$> (x .:? "serverValidationConfigurations" .!= mempty)
            <*> (x .:? "serverGroupId")
      )

instance Hashable ServerGroupValidationConfiguration

instance NFData ServerGroupValidationConfiguration

instance ToJSON ServerGroupValidationConfiguration where
  toJSON ServerGroupValidationConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverValidationConfigurations" .=)
              <$> _sgvcServerValidationConfigurations,
            ("serverGroupId" .=) <$> _sgvcServerGroupId
          ]
      )
