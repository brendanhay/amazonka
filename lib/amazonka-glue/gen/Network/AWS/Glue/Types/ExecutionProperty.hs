{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExecutionProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExecutionProperty where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An execution property of a job.
--
--
--
-- /See:/ 'executionProperty' smart constructor.
newtype ExecutionProperty = ExecutionProperty'
  { _epMaxConcurrentRuns ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epMaxConcurrentRuns' - The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
executionProperty ::
  ExecutionProperty
executionProperty =
  ExecutionProperty' {_epMaxConcurrentRuns = Nothing}

-- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
epMaxConcurrentRuns :: Lens' ExecutionProperty (Maybe Int)
epMaxConcurrentRuns = lens _epMaxConcurrentRuns (\s a -> s {_epMaxConcurrentRuns = a})

instance FromJSON ExecutionProperty where
  parseJSON =
    withObject
      "ExecutionProperty"
      (\x -> ExecutionProperty' <$> (x .:? "MaxConcurrentRuns"))

instance Hashable ExecutionProperty

instance NFData ExecutionProperty

instance ToJSON ExecutionProperty where
  toJSON ExecutionProperty' {..} =
    object
      (catMaybes [("MaxConcurrentRuns" .=) <$> _epMaxConcurrentRuns])
