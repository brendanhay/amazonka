{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EndEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.EndEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | A message that indicates the request is complete and no more messages will be sent. You should not assume that the request is complete until the client receives an @EndEvent@ .
--
--
--
-- /See:/ 'endEvent' smart constructor.
data EndEvent = EndEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndEvent' with the minimum fields required to make a request.
endEvent ::
  EndEvent
endEvent = EndEvent'

instance FromXML EndEvent where
  parseXML = const (pure EndEvent')

instance Hashable EndEvent

instance NFData EndEvent
