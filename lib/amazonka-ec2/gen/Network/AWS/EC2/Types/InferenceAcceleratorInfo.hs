{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InferenceAcceleratorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceAcceleratorInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InferenceDeviceInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Inference accelerators for the instance type.
--
--
--
-- /See:/ 'inferenceAcceleratorInfo' smart constructor.
newtype InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { _iaiAccelerators ::
      Maybe [InferenceDeviceInfo]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InferenceAcceleratorInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaiAccelerators' - Describes the Inference accelerators for the instance type.
inferenceAcceleratorInfo ::
  InferenceAcceleratorInfo
inferenceAcceleratorInfo =
  InferenceAcceleratorInfo' {_iaiAccelerators = Nothing}

-- | Describes the Inference accelerators for the instance type.
iaiAccelerators :: Lens' InferenceAcceleratorInfo [InferenceDeviceInfo]
iaiAccelerators = lens _iaiAccelerators (\s a -> s {_iaiAccelerators = a}) . _Default . _Coerce

instance FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      <$> (x .@? "accelerators" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable InferenceAcceleratorInfo

instance NFData InferenceAcceleratorInfo
