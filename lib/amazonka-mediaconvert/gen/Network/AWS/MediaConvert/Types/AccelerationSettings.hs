{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AccelerationMode
import Network.AWS.Prelude

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /See:/ 'accelerationSettings' smart constructor.
newtype AccelerationSettings = AccelerationSettings'
  { _asMode ::
      AccelerationMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccelerationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asMode' - Specify the conditions when the service will run your job with accelerated transcoding.
accelerationSettings ::
  -- | 'asMode'
  AccelerationMode ->
  AccelerationSettings
accelerationSettings pMode_ =
  AccelerationSettings' {_asMode = pMode_}

-- | Specify the conditions when the service will run your job with accelerated transcoding.
asMode :: Lens' AccelerationSettings AccelerationMode
asMode = lens _asMode (\s a -> s {_asMode = a})

instance FromJSON AccelerationSettings where
  parseJSON =
    withObject
      "AccelerationSettings"
      (\x -> AccelerationSettings' <$> (x .: "mode"))

instance Hashable AccelerationSettings

instance NFData AccelerationSettings

instance ToJSON AccelerationSettings where
  toJSON AccelerationSettings' {..} =
    object (catMaybes [Just ("mode" .= _asMode)])
