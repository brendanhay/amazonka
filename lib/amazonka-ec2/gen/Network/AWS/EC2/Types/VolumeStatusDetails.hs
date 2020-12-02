{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeStatusName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a volume status.
--
--
--
-- /See:/ 'volumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { _vsdStatus ::
      !(Maybe Text),
    _vsdName :: !(Maybe VolumeStatusName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsdStatus' - The intended status of the volume status.
--
-- * 'vsdName' - The name of the volume status.
volumeStatusDetails ::
  VolumeStatusDetails
volumeStatusDetails =
  VolumeStatusDetails' {_vsdStatus = Nothing, _vsdName = Nothing}

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\s a -> s {_vsdStatus = a})

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\s a -> s {_vsdName = a})

instance FromXML VolumeStatusDetails where
  parseXML x =
    VolumeStatusDetails' <$> (x .@? "status") <*> (x .@? "name")

instance Hashable VolumeStatusDetails

instance NFData VolumeStatusDetails
