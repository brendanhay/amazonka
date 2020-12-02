{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ResourcePendingMaintenanceActions where

import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies an AWS DMS resource and any pending actions for it.
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
-- * 'rpmaPendingMaintenanceActionDetails' - Detailed information about the pending maintenance action.
--
-- * 'rpmaResourceIdentifier' - The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
resourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails =
        Nothing,
      _rpmaResourceIdentifier = Nothing
    }

-- | Detailed information about the pending maintenance action.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\s a -> s {_rpmaPendingMaintenanceActionDetails = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\s a -> s {_rpmaResourceIdentifier = a})

instance FromJSON ResourcePendingMaintenanceActions where
  parseJSON =
    withObject
      "ResourcePendingMaintenanceActions"
      ( \x ->
          ResourcePendingMaintenanceActions'
            <$> (x .:? "PendingMaintenanceActionDetails" .!= mempty)
            <*> (x .:? "ResourceIdentifier")
      )

instance Hashable ResourcePendingMaintenanceActions

instance NFData ResourcePendingMaintenanceActions
