{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.OutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The output configuration properties for a batch translation job.
--
--
--
-- /See:/ 'outputDataConfig' smart constructor.
newtype OutputDataConfig = OutputDataConfig' {_odcS3URI :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odcS3URI' - The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
outputDataConfig ::
  -- | 'odcS3URI'
  Text ->
  OutputDataConfig
outputDataConfig pS3URI_ = OutputDataConfig' {_odcS3URI = pS3URI_}

-- | The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
odcS3URI :: Lens' OutputDataConfig Text
odcS3URI = lens _odcS3URI (\s a -> s {_odcS3URI = a})

instance FromJSON OutputDataConfig where
  parseJSON =
    withObject
      "OutputDataConfig"
      (\x -> OutputDataConfig' <$> (x .: "S3Uri"))

instance Hashable OutputDataConfig

instance NFData OutputDataConfig

instance ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    object (catMaybes [Just ("S3Uri" .= _odcS3URI)])
