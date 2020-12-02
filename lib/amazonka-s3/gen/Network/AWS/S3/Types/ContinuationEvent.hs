{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ContinuationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ContinuationEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- |
--
--
--
-- /See:/ 'continuationEvent' smart constructor.
data ContinuationEvent = ContinuationEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContinuationEvent' with the minimum fields required to make a request.
continuationEvent ::
  ContinuationEvent
continuationEvent = ContinuationEvent'

instance FromXML ContinuationEvent where
  parseXML = const (pure ContinuationEvent')

instance Hashable ContinuationEvent

instance NFData ContinuationEvent
