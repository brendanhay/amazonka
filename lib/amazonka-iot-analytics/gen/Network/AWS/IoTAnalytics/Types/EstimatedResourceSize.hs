{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.EstimatedResourceSize where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The estimated size of the resource.
--
--
--
-- /See:/ 'estimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { _ersEstimatedOn ::
      !(Maybe POSIX),
    _ersEstimatedSizeInBytes :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EstimatedResourceSize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ersEstimatedOn' - The time when the estimate of the size of the resource was made.
--
-- * 'ersEstimatedSizeInBytes' - The estimated size of the resource, in bytes.
estimatedResourceSize ::
  EstimatedResourceSize
estimatedResourceSize =
  EstimatedResourceSize'
    { _ersEstimatedOn = Nothing,
      _ersEstimatedSizeInBytes = Nothing
    }

-- | The time when the estimate of the size of the resource was made.
ersEstimatedOn :: Lens' EstimatedResourceSize (Maybe UTCTime)
ersEstimatedOn = lens _ersEstimatedOn (\s a -> s {_ersEstimatedOn = a}) . mapping _Time

-- | The estimated size of the resource, in bytes.
ersEstimatedSizeInBytes :: Lens' EstimatedResourceSize (Maybe Double)
ersEstimatedSizeInBytes = lens _ersEstimatedSizeInBytes (\s a -> s {_ersEstimatedSizeInBytes = a})

instance FromJSON EstimatedResourceSize where
  parseJSON =
    withObject
      "EstimatedResourceSize"
      ( \x ->
          EstimatedResourceSize'
            <$> (x .:? "estimatedOn") <*> (x .:? "estimatedSizeInBytes")
      )

instance Hashable EstimatedResourceSize

instance NFData EstimatedResourceSize
