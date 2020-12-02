{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionModifications where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LoadPermissionRequest
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes modifications to the load permissions of an Amazon FPGA image (AFI).
--
--
--
-- /See:/ 'loadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { _lpmRemove ::
      !(Maybe [LoadPermissionRequest]),
    _lpmAdd ::
      !(Maybe [LoadPermissionRequest])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadPermissionModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpmRemove' - The load permissions to remove.
--
-- * 'lpmAdd' - The load permissions to add.
loadPermissionModifications ::
  LoadPermissionModifications
loadPermissionModifications =
  LoadPermissionModifications'
    { _lpmRemove = Nothing,
      _lpmAdd = Nothing
    }

-- | The load permissions to remove.
lpmRemove :: Lens' LoadPermissionModifications [LoadPermissionRequest]
lpmRemove = lens _lpmRemove (\s a -> s {_lpmRemove = a}) . _Default . _Coerce

-- | The load permissions to add.
lpmAdd :: Lens' LoadPermissionModifications [LoadPermissionRequest]
lpmAdd = lens _lpmAdd (\s a -> s {_lpmAdd = a}) . _Default . _Coerce

instance Hashable LoadPermissionModifications

instance NFData LoadPermissionModifications

instance ToQuery LoadPermissionModifications where
  toQuery LoadPermissionModifications' {..} =
    mconcat
      [ toQuery (toQueryList "Remove" <$> _lpmRemove),
        toQuery (toQueryList "Add" <$> _lpmAdd)
      ]
