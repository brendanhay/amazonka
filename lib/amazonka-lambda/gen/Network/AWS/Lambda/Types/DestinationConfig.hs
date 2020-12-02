{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DestinationConfig where

import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration object that specifies the destination of an event after Lambda processes it.
--
--
--
-- /See:/ 'destinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { _dcOnSuccess ::
      !(Maybe OnSuccess),
    _dcOnFailure :: !(Maybe OnFailure)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcOnSuccess' - The destination configuration for successful invocations.
--
-- * 'dcOnFailure' - The destination configuration for failed invocations.
destinationConfig ::
  DestinationConfig
destinationConfig =
  DestinationConfig'
    { _dcOnSuccess = Nothing,
      _dcOnFailure = Nothing
    }

-- | The destination configuration for successful invocations.
dcOnSuccess :: Lens' DestinationConfig (Maybe OnSuccess)
dcOnSuccess = lens _dcOnSuccess (\s a -> s {_dcOnSuccess = a})

-- | The destination configuration for failed invocations.
dcOnFailure :: Lens' DestinationConfig (Maybe OnFailure)
dcOnFailure = lens _dcOnFailure (\s a -> s {_dcOnFailure = a})

instance FromJSON DestinationConfig where
  parseJSON =
    withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig' <$> (x .:? "OnSuccess") <*> (x .:? "OnFailure")
      )

instance Hashable DestinationConfig

instance NFData DestinationConfig

instance ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    object
      ( catMaybes
          [ ("OnSuccess" .=) <$> _dcOnSuccess,
            ("OnFailure" .=) <$> _dcOnFailure
          ]
      )
