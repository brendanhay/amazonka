{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the gateway group.
--
--
--
-- /See:/ 'gatewayGroup' smart constructor.
data GatewayGroup = GatewayGroup'
  { _ggARN :: !(Maybe Text),
    _ggName :: !(Maybe Text),
    _ggDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GatewayGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggARN' - The ARN of the gateway group.
--
-- * 'ggName' - The name of the gateway group.
--
-- * 'ggDescription' - The description of the gateway group.
gatewayGroup ::
  GatewayGroup
gatewayGroup =
  GatewayGroup'
    { _ggARN = Nothing,
      _ggName = Nothing,
      _ggDescription = Nothing
    }

-- | The ARN of the gateway group.
ggARN :: Lens' GatewayGroup (Maybe Text)
ggARN = lens _ggARN (\s a -> s {_ggARN = a})

-- | The name of the gateway group.
ggName :: Lens' GatewayGroup (Maybe Text)
ggName = lens _ggName (\s a -> s {_ggName = a})

-- | The description of the gateway group.
ggDescription :: Lens' GatewayGroup (Maybe Text)
ggDescription = lens _ggDescription (\s a -> s {_ggDescription = a})

instance FromJSON GatewayGroup where
  parseJSON =
    withObject
      "GatewayGroup"
      ( \x ->
          GatewayGroup'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Description")
      )

instance Hashable GatewayGroup

instance NFData GatewayGroup
