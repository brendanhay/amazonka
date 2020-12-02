{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMServerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VMServerAddress where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a VM server location.
--
--
--
-- /See:/ 'vMServerAddress' smart constructor.
data VMServerAddress = VMServerAddress'
  { _vmsaVmManagerId ::
      !(Maybe Text),
    _vmsaVmId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VMServerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmsaVmManagerId' - The ID of the VM manager.
--
-- * 'vmsaVmId' - The ID of the VM.
vMServerAddress ::
  VMServerAddress
vMServerAddress =
  VMServerAddress' {_vmsaVmManagerId = Nothing, _vmsaVmId = Nothing}

-- | The ID of the VM manager.
vmsaVmManagerId :: Lens' VMServerAddress (Maybe Text)
vmsaVmManagerId = lens _vmsaVmManagerId (\s a -> s {_vmsaVmManagerId = a})

-- | The ID of the VM.
vmsaVmId :: Lens' VMServerAddress (Maybe Text)
vmsaVmId = lens _vmsaVmId (\s a -> s {_vmsaVmId = a})

instance FromJSON VMServerAddress where
  parseJSON =
    withObject
      "VMServerAddress"
      ( \x ->
          VMServerAddress' <$> (x .:? "vmManagerId") <*> (x .:? "vmId")
      )

instance Hashable VMServerAddress

instance NFData VMServerAddress

instance ToJSON VMServerAddress where
  toJSON VMServerAddress' {..} =
    object
      ( catMaybes
          [ ("vmManagerId" .=) <$> _vmsaVmManagerId,
            ("vmId" .=) <$> _vmsaVmId
          ]
      )
