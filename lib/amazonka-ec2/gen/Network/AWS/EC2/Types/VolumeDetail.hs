{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeDetail where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EBS volume.
--
--
--
-- /See:/ 'volumeDetail' smart constructor.
newtype VolumeDetail = VolumeDetail' {_vdSize :: Integer}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdSize' - The size of the volume, in GiB.
volumeDetail ::
  -- | 'vdSize'
  Integer ->
  VolumeDetail
volumeDetail pSize_ = VolumeDetail' {_vdSize = pSize_}

-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\s a -> s {_vdSize = a})

instance Hashable VolumeDetail

instance NFData VolumeDetail

instance ToQuery VolumeDetail where
  toQuery VolumeDetail' {..} = mconcat ["Size" =: _vdSize]
