{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointConfigSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides summary information for an endpoint configuration.
--
--
--
-- /See:/ 'endpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { _ecsEndpointConfigName ::
      !Text,
    _ecsEndpointConfigARN :: !Text,
    _ecsCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecsEndpointConfigName' - The name of the endpoint configuration.
--
-- * 'ecsEndpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- * 'ecsCreationTime' - A timestamp that shows when the endpoint configuration was created.
endpointConfigSummary ::
  -- | 'ecsEndpointConfigName'
  Text ->
  -- | 'ecsEndpointConfigARN'
  Text ->
  -- | 'ecsCreationTime'
  UTCTime ->
  EndpointConfigSummary
endpointConfigSummary
  pEndpointConfigName_
  pEndpointConfigARN_
  pCreationTime_ =
    EndpointConfigSummary'
      { _ecsEndpointConfigName =
          pEndpointConfigName_,
        _ecsEndpointConfigARN = pEndpointConfigARN_,
        _ecsCreationTime = _Time # pCreationTime_
      }

-- | The name of the endpoint configuration.
ecsEndpointConfigName :: Lens' EndpointConfigSummary Text
ecsEndpointConfigName = lens _ecsEndpointConfigName (\s a -> s {_ecsEndpointConfigName = a})

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
ecsEndpointConfigARN :: Lens' EndpointConfigSummary Text
ecsEndpointConfigARN = lens _ecsEndpointConfigARN (\s a -> s {_ecsEndpointConfigARN = a})

-- | A timestamp that shows when the endpoint configuration was created.
ecsCreationTime :: Lens' EndpointConfigSummary UTCTime
ecsCreationTime = lens _ecsCreationTime (\s a -> s {_ecsCreationTime = a}) . _Time

instance FromJSON EndpointConfigSummary where
  parseJSON =
    withObject
      "EndpointConfigSummary"
      ( \x ->
          EndpointConfigSummary'
            <$> (x .: "EndpointConfigName")
            <*> (x .: "EndpointConfigArn")
            <*> (x .: "CreationTime")
      )

instance Hashable EndpointConfigSummary

instance NFData EndpointConfigSummary
