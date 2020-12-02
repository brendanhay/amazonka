{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EnvironmentVariable where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an app's environment variable.
--
--
--
-- /See:/ 'environmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { _evSecure ::
      !(Maybe Bool),
    _evKey :: !Text,
    _evValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evSecure' - (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
--
-- * 'evKey' - (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
--
-- * 'evValue' - (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
environmentVariable ::
  -- | 'evKey'
  Text ->
  -- | 'evValue'
  Text ->
  EnvironmentVariable
environmentVariable pKey_ pValue_ =
  EnvironmentVariable'
    { _evSecure = Nothing,
      _evKey = pKey_,
      _evValue = pValue_
    }

-- | (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
evSecure :: Lens' EnvironmentVariable (Maybe Bool)
evSecure = lens _evSecure (\s a -> s {_evSecure = a})

-- | (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
evKey :: Lens' EnvironmentVariable Text
evKey = lens _evKey (\s a -> s {_evKey = a})

-- | (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\s a -> s {_evValue = a})

instance FromJSON EnvironmentVariable where
  parseJSON =
    withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            <$> (x .:? "Secure") <*> (x .: "Key") <*> (x .: "Value")
      )

instance Hashable EnvironmentVariable

instance NFData EnvironmentVariable

instance ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    object
      ( catMaybes
          [ ("Secure" .=) <$> _evSecure,
            Just ("Key" .= _evKey),
            Just ("Value" .= _evValue)
          ]
      )
