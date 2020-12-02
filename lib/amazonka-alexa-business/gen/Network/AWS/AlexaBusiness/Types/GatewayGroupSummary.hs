{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary of a gateway group.
--
--
--
-- /See:/ 'gatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { _ggsARN ::
      !(Maybe Text),
    _ggsName :: !(Maybe Text),
    _ggsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GatewayGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggsARN' - The ARN of the gateway group.
--
-- * 'ggsName' - The name of the gateway group.
--
-- * 'ggsDescription' - The description of the gateway group.
gatewayGroupSummary ::
  GatewayGroupSummary
gatewayGroupSummary =
  GatewayGroupSummary'
    { _ggsARN = Nothing,
      _ggsName = Nothing,
      _ggsDescription = Nothing
    }

-- | The ARN of the gateway group.
ggsARN :: Lens' GatewayGroupSummary (Maybe Text)
ggsARN = lens _ggsARN (\s a -> s {_ggsARN = a})

-- | The name of the gateway group.
ggsName :: Lens' GatewayGroupSummary (Maybe Text)
ggsName = lens _ggsName (\s a -> s {_ggsName = a})

-- | The description of the gateway group.
ggsDescription :: Lens' GatewayGroupSummary (Maybe Text)
ggsDescription = lens _ggsDescription (\s a -> s {_ggsDescription = a})

instance FromJSON GatewayGroupSummary where
  parseJSON =
    withObject
      "GatewayGroupSummary"
      ( \x ->
          GatewayGroupSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Description")
      )

instance Hashable GatewayGroupSummary

instance NFData GatewayGroupSummary
