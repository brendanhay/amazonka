{-# LANGUAGE DeriveGeneric #-}

-- Module      : Network.AWS.AutoScaling.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.Internal where

import GHC.Generics
import Network.AWS.Data
import Network.AWS.Prelude

data ErrorType
    = Receiver
    | Sender
      deriving (Eq, Ord, Enum, Show, Generic)

instance FromXML ErrorType

data Message = Message
    { _msgType    :: !ErrorType
    , _msgCode    :: Text
    , _msgMessage :: Text
    } deriving (Eq, Ord, Show, Generic)

instance FromXML Message

data AutoScalingError = AutoScalingError
    { _errError     :: Message
    , _errRequestId :: Text
    } deriving (Eq, Show, Generic)

instance FromXML AutoScalingError
