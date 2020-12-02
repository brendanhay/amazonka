{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ResourcePendingMaintenanceActions where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.PendingMaintenanceAction

-- | Describes the pending maintenance actions for a resource.
--
--
--
-- /See:/ 'resourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { _rpmaPendingMaintenanceActionDetails ::
      !( Maybe
           [PendingMaintenanceAction]
       ),
    _rpmaResourceIdentifier ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourcePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmaPendingMaintenanceActionDetails' - A list that provides details about the pending maintenance actions for the resource.
--
-- * 'rpmaResourceIdentifier' - The ARN of the resource that has pending maintenance actions.
resourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails =
        Nothing,
      _rpmaResourceIdentifier = Nothing
    }

-- | A list that provides details about the pending maintenance actions for the resource.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\s a -> s {_rpmaPendingMaintenanceActionDetails = a}) . _Default . _Coerce

-- | The ARN of the resource that has pending maintenance actions.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\s a -> s {_rpmaResourceIdentifier = a})

instance FromXML ResourcePendingMaintenanceActions where
  parseXML x =
    ResourcePendingMaintenanceActions'
      <$> ( x .@? "PendingMaintenanceActionDetails" .!@ mempty
              >>= may (parseXMLList "PendingMaintenanceAction")
          )
      <*> (x .@? "ResourceIdentifier")

instance Hashable ResourcePendingMaintenanceActions

instance NFData ResourcePendingMaintenanceActions
