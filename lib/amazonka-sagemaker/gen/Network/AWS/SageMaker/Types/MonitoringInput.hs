{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.EndpointInput

-- | The inputs for a monitoring job.
--
--
--
-- /See:/ 'monitoringInput' smart constructor.
newtype MonitoringInput = MonitoringInput'
  { _miEndpointInput ::
      EndpointInput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miEndpointInput' - The endpoint for a monitoring job.
monitoringInput ::
  -- | 'miEndpointInput'
  EndpointInput ->
  MonitoringInput
monitoringInput pEndpointInput_ =
  MonitoringInput' {_miEndpointInput = pEndpointInput_}

-- | The endpoint for a monitoring job.
miEndpointInput :: Lens' MonitoringInput EndpointInput
miEndpointInput = lens _miEndpointInput (\s a -> s {_miEndpointInput = a})

instance FromJSON MonitoringInput where
  parseJSON =
    withObject
      "MonitoringInput"
      (\x -> MonitoringInput' <$> (x .: "EndpointInput"))

instance Hashable MonitoringInput

instance NFData MonitoringInput

instance ToJSON MonitoringInput where
  toJSON MonitoringInput' {..} =
    object (catMaybes [Just ("EndpointInput" .= _miEndpointInput)])
