{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchArrayProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
--
--
-- /See:/ 'batchArrayProperties' smart constructor.
newtype BatchArrayProperties = BatchArrayProperties'
  { _bapSize ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchArrayProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bapSize' - The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
batchArrayProperties ::
  BatchArrayProperties
batchArrayProperties = BatchArrayProperties' {_bapSize = Nothing}

-- | The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
bapSize :: Lens' BatchArrayProperties (Maybe Int)
bapSize = lens _bapSize (\s a -> s {_bapSize = a})

instance FromJSON BatchArrayProperties where
  parseJSON =
    withObject
      "BatchArrayProperties"
      (\x -> BatchArrayProperties' <$> (x .:? "Size"))

instance Hashable BatchArrayProperties

instance NFData BatchArrayProperties

instance ToJSON BatchArrayProperties where
  toJSON BatchArrayProperties' {..} =
    object (catMaybes [("Size" .=) <$> _bapSize])
