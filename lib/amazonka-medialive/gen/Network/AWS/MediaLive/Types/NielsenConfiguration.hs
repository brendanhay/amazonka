{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NielsenConfiguration where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
import Network.AWS.Prelude

-- | Nielsen Configuration
--
-- /See:/ 'nielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { _ncDistributorId ::
      !(Maybe Text),
    _ncNielsenPcmToId3Tagging ::
      !(Maybe NielsenPcmToId3TaggingState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncDistributorId' - Enter the Distributor ID assigned to your organization by Nielsen.
--
-- * 'ncNielsenPcmToId3Tagging' - Enables Nielsen PCM to ID3 tagging
nielsenConfiguration ::
  NielsenConfiguration
nielsenConfiguration =
  NielsenConfiguration'
    { _ncDistributorId = Nothing,
      _ncNielsenPcmToId3Tagging = Nothing
    }

-- | Enter the Distributor ID assigned to your organization by Nielsen.
ncDistributorId :: Lens' NielsenConfiguration (Maybe Text)
ncDistributorId = lens _ncDistributorId (\s a -> s {_ncDistributorId = a})

-- | Enables Nielsen PCM to ID3 tagging
ncNielsenPcmToId3Tagging :: Lens' NielsenConfiguration (Maybe NielsenPcmToId3TaggingState)
ncNielsenPcmToId3Tagging = lens _ncNielsenPcmToId3Tagging (\s a -> s {_ncNielsenPcmToId3Tagging = a})

instance FromJSON NielsenConfiguration where
  parseJSON =
    withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            <$> (x .:? "distributorId") <*> (x .:? "nielsenPcmToId3Tagging")
      )

instance Hashable NielsenConfiguration

instance NFData NielsenConfiguration

instance ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    object
      ( catMaybes
          [ ("distributorId" .=) <$> _ncDistributorId,
            ("nielsenPcmToId3Tagging" .=) <$> _ncNielsenPcmToId3Tagging
          ]
      )
