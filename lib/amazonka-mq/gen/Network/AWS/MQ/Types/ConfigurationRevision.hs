{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ConfigurationRevision where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the specified configuration revision.
--
-- /See:/ 'configurationRevision' smart constructor.
data ConfigurationRevision = ConfigurationRevision'
  { _crCreated ::
      !(Maybe POSIX),
    _crRevision :: !(Maybe Int),
    _crDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crCreated' - Required. The date and time of the configuration revision.
--
-- * 'crRevision' - Required. The revision number of the configuration.
--
-- * 'crDescription' - The description of the configuration revision.
configurationRevision ::
  ConfigurationRevision
configurationRevision =
  ConfigurationRevision'
    { _crCreated = Nothing,
      _crRevision = Nothing,
      _crDescription = Nothing
    }

-- | Required. The date and time of the configuration revision.
crCreated :: Lens' ConfigurationRevision (Maybe UTCTime)
crCreated = lens _crCreated (\s a -> s {_crCreated = a}) . mapping _Time

-- | Required. The revision number of the configuration.
crRevision :: Lens' ConfigurationRevision (Maybe Int)
crRevision = lens _crRevision (\s a -> s {_crRevision = a})

-- | The description of the configuration revision.
crDescription :: Lens' ConfigurationRevision (Maybe Text)
crDescription = lens _crDescription (\s a -> s {_crDescription = a})

instance FromJSON ConfigurationRevision where
  parseJSON =
    withObject
      "ConfigurationRevision"
      ( \x ->
          ConfigurationRevision'
            <$> (x .:? "created") <*> (x .:? "revision") <*> (x .:? "description")
      )

instance Hashable ConfigurationRevision

instance NFData ConfigurationRevision
