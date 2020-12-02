{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeStatusDetails
import Network.AWS.EC2.Types.VolumeStatusInfoStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of a volume.
--
--
--
-- /See:/ 'volumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { _vsiStatus ::
      !(Maybe VolumeStatusInfoStatus),
    _vsiDetails :: !(Maybe [VolumeStatusDetails])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsiStatus' - The status of the volume.
--
-- * 'vsiDetails' - The details of the volume status.
volumeStatusInfo ::
  VolumeStatusInfo
volumeStatusInfo =
  VolumeStatusInfo' {_vsiStatus = Nothing, _vsiDetails = Nothing}

-- | The status of the volume.
vsiStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsiStatus = lens _vsiStatus (\s a -> s {_vsiStatus = a})

-- | The details of the volume status.
vsiDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsiDetails = lens _vsiDetails (\s a -> s {_vsiDetails = a}) . _Default . _Coerce

instance FromXML VolumeStatusInfo where
  parseXML x =
    VolumeStatusInfo'
      <$> (x .@? "status")
      <*> (x .@? "details" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable VolumeStatusInfo

instance NFData VolumeStatusInfo
