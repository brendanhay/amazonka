{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermissionModifications where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CreateVolumePermission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes modifications to the list of create volume permissions for a volume.
--
--
--
-- /See:/ 'createVolumePermissionModifications' smart constructor.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'
  { _cvpmRemove ::
      !( Maybe
           [CreateVolumePermission]
       ),
    _cvpmAdd ::
      !( Maybe
           [CreateVolumePermission]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVolumePermissionModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpmRemove' - Removes the specified AWS account ID or group from the list.
--
-- * 'cvpmAdd' - Adds the specified AWS account ID or group to the list.
createVolumePermissionModifications ::
  CreateVolumePermissionModifications
createVolumePermissionModifications =
  CreateVolumePermissionModifications'
    { _cvpmRemove = Nothing,
      _cvpmAdd = Nothing
    }

-- | Removes the specified AWS account ID or group from the list.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\s a -> s {_cvpmRemove = a}) . _Default . _Coerce

-- | Adds the specified AWS account ID or group to the list.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\s a -> s {_cvpmAdd = a}) . _Default . _Coerce

instance Hashable CreateVolumePermissionModifications

instance NFData CreateVolumePermissionModifications

instance ToQuery CreateVolumePermissionModifications where
  toQuery CreateVolumePermissionModifications' {..} =
    mconcat
      [ toQuery (toQueryList "Remove" <$> _cvpmRemove),
        toQuery (toQueryList "Add" <$> _cvpmAdd)
      ]
