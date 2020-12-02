{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
import Network.AWS.SageMaker.Types.ProcessingS3InputMode

-- | Input object for the endpoint
--
--
--
-- /See:/ 'endpointInput' smart constructor.
data EndpointInput = EndpointInput'
  { _eiS3DataDistributionType ::
      !(Maybe ProcessingS3DataDistributionType),
    _eiS3InputMode :: !(Maybe ProcessingS3InputMode),
    _eiEndpointName :: !Text,
    _eiLocalPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiS3DataDistributionType' - Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@
--
-- * 'eiS3InputMode' - Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
--
-- * 'eiEndpointName' - An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
--
-- * 'eiLocalPath' - Path to the filesystem where the endpoint data is available to the container.
endpointInput ::
  -- | 'eiEndpointName'
  Text ->
  -- | 'eiLocalPath'
  Text ->
  EndpointInput
endpointInput pEndpointName_ pLocalPath_ =
  EndpointInput'
    { _eiS3DataDistributionType = Nothing,
      _eiS3InputMode = Nothing,
      _eiEndpointName = pEndpointName_,
      _eiLocalPath = pLocalPath_
    }

-- | Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@
eiS3DataDistributionType :: Lens' EndpointInput (Maybe ProcessingS3DataDistributionType)
eiS3DataDistributionType = lens _eiS3DataDistributionType (\s a -> s {_eiS3DataDistributionType = a})

-- | Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
eiS3InputMode :: Lens' EndpointInput (Maybe ProcessingS3InputMode)
eiS3InputMode = lens _eiS3InputMode (\s a -> s {_eiS3InputMode = a})

-- | An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
eiEndpointName :: Lens' EndpointInput Text
eiEndpointName = lens _eiEndpointName (\s a -> s {_eiEndpointName = a})

-- | Path to the filesystem where the endpoint data is available to the container.
eiLocalPath :: Lens' EndpointInput Text
eiLocalPath = lens _eiLocalPath (\s a -> s {_eiLocalPath = a})

instance FromJSON EndpointInput where
  parseJSON =
    withObject
      "EndpointInput"
      ( \x ->
          EndpointInput'
            <$> (x .:? "S3DataDistributionType")
            <*> (x .:? "S3InputMode")
            <*> (x .: "EndpointName")
            <*> (x .: "LocalPath")
      )

instance Hashable EndpointInput

instance NFData EndpointInput

instance ToJSON EndpointInput where
  toJSON EndpointInput' {..} =
    object
      ( catMaybes
          [ ("S3DataDistributionType" .=) <$> _eiS3DataDistributionType,
            ("S3InputMode" .=) <$> _eiS3InputMode,
            Just ("EndpointName" .= _eiEndpointName),
            Just ("LocalPath" .= _eiLocalPath)
          ]
      )
