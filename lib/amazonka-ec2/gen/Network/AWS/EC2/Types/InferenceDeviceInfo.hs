{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InferenceDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceDeviceInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Inference accelerators for the instance type.
--
--
--
-- /See:/ 'inferenceDeviceInfo' smart constructor.
data InferenceDeviceInfo = InferenceDeviceInfo'
  { _idiManufacturer ::
      !(Maybe Text),
    _idiCount :: !(Maybe Int),
    _idiName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InferenceDeviceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idiManufacturer' - The manufacturer of the Inference accelerator.
--
-- * 'idiCount' - The number of Inference accelerators for the instance type.
--
-- * 'idiName' - The name of the Inference accelerator.
inferenceDeviceInfo ::
  InferenceDeviceInfo
inferenceDeviceInfo =
  InferenceDeviceInfo'
    { _idiManufacturer = Nothing,
      _idiCount = Nothing,
      _idiName = Nothing
    }

-- | The manufacturer of the Inference accelerator.
idiManufacturer :: Lens' InferenceDeviceInfo (Maybe Text)
idiManufacturer = lens _idiManufacturer (\s a -> s {_idiManufacturer = a})

-- | The number of Inference accelerators for the instance type.
idiCount :: Lens' InferenceDeviceInfo (Maybe Int)
idiCount = lens _idiCount (\s a -> s {_idiCount = a})

-- | The name of the Inference accelerator.
idiName :: Lens' InferenceDeviceInfo (Maybe Text)
idiName = lens _idiName (\s a -> s {_idiName = a})

instance FromXML InferenceDeviceInfo where
  parseXML x =
    InferenceDeviceInfo'
      <$> (x .@? "manufacturer") <*> (x .@? "count") <*> (x .@? "name")

instance Hashable InferenceDeviceInfo

instance NFData InferenceDeviceInfo
