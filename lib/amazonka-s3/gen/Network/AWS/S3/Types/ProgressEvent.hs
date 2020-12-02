{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ProgressEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ProgressEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Progress

-- | This data type contains information about the progress event of an operation.
--
--
--
-- /See:/ 'progressEvent' smart constructor.
newtype ProgressEvent = ProgressEvent'
  { _peDetails ::
      Maybe Progress
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProgressEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peDetails' - The Progress event details.
progressEvent ::
  ProgressEvent
progressEvent = ProgressEvent' {_peDetails = Nothing}

-- | The Progress event details.
peDetails :: Lens' ProgressEvent (Maybe Progress)
peDetails = lens _peDetails (\s a -> s {_peDetails = a})

instance FromXML ProgressEvent where
  parseXML x = ProgressEvent' <$> (x .@? "Details")

instance Hashable ProgressEvent

instance NFData ProgressEvent
