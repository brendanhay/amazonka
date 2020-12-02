{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.VersioningConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the versioning of dataset contents.
--
--
--
-- /See:/ 'versioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { _vcUnlimited ::
      !(Maybe Bool),
    _vcMaxVersions :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcUnlimited' - If true, unlimited versions of dataset contents are kept.
--
-- * 'vcMaxVersions' - How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
versioningConfiguration ::
  VersioningConfiguration
versioningConfiguration =
  VersioningConfiguration'
    { _vcUnlimited = Nothing,
      _vcMaxVersions = Nothing
    }

-- | If true, unlimited versions of dataset contents are kept.
vcUnlimited :: Lens' VersioningConfiguration (Maybe Bool)
vcUnlimited = lens _vcUnlimited (\s a -> s {_vcUnlimited = a})

-- | How many versions of dataset contents are kept. The @unlimited@ parameter must be @false@ .
vcMaxVersions :: Lens' VersioningConfiguration (Maybe Natural)
vcMaxVersions = lens _vcMaxVersions (\s a -> s {_vcMaxVersions = a}) . mapping _Nat

instance FromJSON VersioningConfiguration where
  parseJSON =
    withObject
      "VersioningConfiguration"
      ( \x ->
          VersioningConfiguration'
            <$> (x .:? "unlimited") <*> (x .:? "maxVersions")
      )

instance Hashable VersioningConfiguration

instance NFData VersioningConfiguration

instance ToJSON VersioningConfiguration where
  toJSON VersioningConfiguration' {..} =
    object
      ( catMaybes
          [ ("unlimited" .=) <$> _vcUnlimited,
            ("maxVersions" .=) <$> _vcMaxVersions
          ]
      )
