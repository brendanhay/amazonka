{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /See:/ 'nielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { _ncBreakoutCode ::
      !(Maybe Nat),
    _ncDistributorId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncBreakoutCode' - Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
--
-- * 'ncDistributorId' - Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
nielsenConfiguration ::
  NielsenConfiguration
nielsenConfiguration =
  NielsenConfiguration'
    { _ncBreakoutCode = Nothing,
      _ncDistributorId = Nothing
    }

-- | Nielsen has discontinued the use of breakout code functionality. If you must include this property, set the value to zero.
ncBreakoutCode :: Lens' NielsenConfiguration (Maybe Natural)
ncBreakoutCode = lens _ncBreakoutCode (\s a -> s {_ncBreakoutCode = a}) . mapping _Nat

-- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
ncDistributorId :: Lens' NielsenConfiguration (Maybe Text)
ncDistributorId = lens _ncDistributorId (\s a -> s {_ncDistributorId = a})

instance FromJSON NielsenConfiguration where
  parseJSON =
    withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            <$> (x .:? "breakoutCode") <*> (x .:? "distributorId")
      )

instance Hashable NielsenConfiguration

instance NFData NielsenConfiguration

instance ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    object
      ( catMaybes
          [ ("breakoutCode" .=) <$> _ncBreakoutCode,
            ("distributorId" .=) <$> _ncDistributorId
          ]
      )
