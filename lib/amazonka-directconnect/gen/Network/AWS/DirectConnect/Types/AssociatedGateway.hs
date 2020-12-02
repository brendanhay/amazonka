{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.AssociatedGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.AssociatedGateway where

import Network.AWS.DirectConnect.Types.GatewayType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the associated gateway.
--
--
--
-- /See:/ 'associatedGateway' smart constructor.
data AssociatedGateway = AssociatedGateway'
  { _agId :: !(Maybe Text),
    _agOwnerAccount :: !(Maybe Text),
    _agRegion :: !(Maybe Text),
    _agType :: !(Maybe GatewayType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociatedGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agId' - The ID of the associated gateway.
--
-- * 'agOwnerAccount' - The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
--
-- * 'agRegion' - The Region where the associated gateway is located.
--
-- * 'agType' - The type of associated gateway.
associatedGateway ::
  AssociatedGateway
associatedGateway =
  AssociatedGateway'
    { _agId = Nothing,
      _agOwnerAccount = Nothing,
      _agRegion = Nothing,
      _agType = Nothing
    }

-- | The ID of the associated gateway.
agId :: Lens' AssociatedGateway (Maybe Text)
agId = lens _agId (\s a -> s {_agId = a})

-- | The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
agOwnerAccount :: Lens' AssociatedGateway (Maybe Text)
agOwnerAccount = lens _agOwnerAccount (\s a -> s {_agOwnerAccount = a})

-- | The Region where the associated gateway is located.
agRegion :: Lens' AssociatedGateway (Maybe Text)
agRegion = lens _agRegion (\s a -> s {_agRegion = a})

-- | The type of associated gateway.
agType :: Lens' AssociatedGateway (Maybe GatewayType)
agType = lens _agType (\s a -> s {_agType = a})

instance FromJSON AssociatedGateway where
  parseJSON =
    withObject
      "AssociatedGateway"
      ( \x ->
          AssociatedGateway'
            <$> (x .:? "id")
            <*> (x .:? "ownerAccount")
            <*> (x .:? "region")
            <*> (x .:? "type")
      )

instance Hashable AssociatedGateway

instance NFData AssociatedGateway
