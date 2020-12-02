{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Sample
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Sample where

import Network.AWS.DeviceFarm.Types.SampleType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a sample of performance data.
--
--
--
-- /See:/ 'sample' smart constructor.
data Sample = Sample'
  { _samArn :: !(Maybe Text),
    _samUrl :: !(Maybe Text),
    _samType :: !(Maybe SampleType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Sample' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samArn' - The sample's ARN.
--
-- * 'samUrl' - The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
--
-- * 'samType' - The sample's type. Must be one of the following values:     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.     * NATIVE_AVG_DRAWTIME     * NATIVE_FPS     * NATIVE_FRAMES     * NATIVE_MAX_DRAWTIME     * NATIVE_MIN_DRAWTIME     * OPENGL_AVG_DRAWTIME     * OPENGL_FPS     * OPENGL_FRAMES     * OPENGL_MAX_DRAWTIME     * OPENGL_MIN_DRAWTIME     * RX     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.     * TX     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
sample ::
  Sample
sample =
  Sample' {_samArn = Nothing, _samUrl = Nothing, _samType = Nothing}

-- | The sample's ARN.
samArn :: Lens' Sample (Maybe Text)
samArn = lens _samArn (\s a -> s {_samArn = a})

-- | The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
samUrl :: Lens' Sample (Maybe Text)
samUrl = lens _samUrl (\s a -> s {_samUrl = a})

-- | The sample's type. Must be one of the following values:     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.     * NATIVE_AVG_DRAWTIME     * NATIVE_FPS     * NATIVE_FRAMES     * NATIVE_MAX_DRAWTIME     * NATIVE_MIN_DRAWTIME     * OPENGL_AVG_DRAWTIME     * OPENGL_FPS     * OPENGL_FRAMES     * OPENGL_MAX_DRAWTIME     * OPENGL_MIN_DRAWTIME     * RX     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.     * TX     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
samType :: Lens' Sample (Maybe SampleType)
samType = lens _samType (\s a -> s {_samType = a})

instance FromJSON Sample where
  parseJSON =
    withObject
      "Sample"
      ( \x ->
          Sample' <$> (x .:? "arn") <*> (x .:? "url") <*> (x .:? "type")
      )

instance Hashable Sample

instance NFData Sample
