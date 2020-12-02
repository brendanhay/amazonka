{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.ParallelDataFormat

-- | Specifies the format and S3 location of the parallel data input file.
--
--
--
-- /See:/ 'parallelDataConfig' smart constructor.
data ParallelDataConfig = ParallelDataConfig'
  { _pdcS3URI :: !Text,
    _pdcFormat :: !ParallelDataFormat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParallelDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdcS3URI' - The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- * 'pdcFormat' - The format of the parallel data input file.
parallelDataConfig ::
  -- | 'pdcS3URI'
  Text ->
  -- | 'pdcFormat'
  ParallelDataFormat ->
  ParallelDataConfig
parallelDataConfig pS3URI_ pFormat_ =
  ParallelDataConfig' {_pdcS3URI = pS3URI_, _pdcFormat = pFormat_}

-- | The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
pdcS3URI :: Lens' ParallelDataConfig Text
pdcS3URI = lens _pdcS3URI (\s a -> s {_pdcS3URI = a})

-- | The format of the parallel data input file.
pdcFormat :: Lens' ParallelDataConfig ParallelDataFormat
pdcFormat = lens _pdcFormat (\s a -> s {_pdcFormat = a})

instance FromJSON ParallelDataConfig where
  parseJSON =
    withObject
      "ParallelDataConfig"
      (\x -> ParallelDataConfig' <$> (x .: "S3Uri") <*> (x .: "Format"))

instance Hashable ParallelDataConfig

instance NFData ParallelDataConfig

instance ToJSON ParallelDataConfig where
  toJSON ParallelDataConfig' {..} =
    object
      ( catMaybes
          [Just ("S3Uri" .= _pdcS3URI), Just ("Format" .= _pdcFormat)]
      )
