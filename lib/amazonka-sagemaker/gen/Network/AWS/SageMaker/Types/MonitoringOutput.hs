{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringS3Output

-- | The output object for a monitoring job.
--
--
--
-- /See:/ 'monitoringOutput' smart constructor.
newtype MonitoringOutput = MonitoringOutput'
  { _moS3Output ::
      MonitoringS3Output
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'moS3Output' - The Amazon S3 storage location where the results of a monitoring job are saved.
monitoringOutput ::
  -- | 'moS3Output'
  MonitoringS3Output ->
  MonitoringOutput
monitoringOutput pS3Output_ =
  MonitoringOutput' {_moS3Output = pS3Output_}

-- | The Amazon S3 storage location where the results of a monitoring job are saved.
moS3Output :: Lens' MonitoringOutput MonitoringS3Output
moS3Output = lens _moS3Output (\s a -> s {_moS3Output = a})

instance FromJSON MonitoringOutput where
  parseJSON =
    withObject
      "MonitoringOutput"
      (\x -> MonitoringOutput' <$> (x .: "S3Output"))

instance Hashable MonitoringOutput

instance NFData MonitoringOutput

instance ToJSON MonitoringOutput where
  toJSON MonitoringOutput' {..} =
    object (catMaybes [Just ("S3Output" .= _moS3Output)])
