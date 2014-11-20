{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.RemoveEventSource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an event source mapping. This means AWS Lambda will no longer
-- invoke the function for events in the associated source. This operation
-- requires permission for the lambda:RemoveEventSource action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_RemoveEventSource.html>
module Network.AWS.Lambda.RemoveEventSource
    (
    -- * Request
      RemoveEventSource
    -- ** Request constructor
    , removeEventSource
    -- ** Request lenses
    , resUUID

    -- * Response
    , RemoveEventSourceResponse
    -- ** Response constructor
    , removeEventSourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype RemoveEventSource = RemoveEventSource
    { _resUUID :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'RemoveEventSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resUUID' @::@ 'Text'
--
removeEventSource :: Text -- ^ 'resUUID'
                  -> RemoveEventSource
removeEventSource p1 = RemoveEventSource
    { _resUUID = p1
    }

-- | The event source mapping ID.
resUUID :: Lens' RemoveEventSource Text
resUUID = lens _resUUID (\s a -> s { _resUUID = a })

data RemoveEventSourceResponse = RemoveEventSourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveEventSourceResponse' constructor.
removeEventSourceResponse :: RemoveEventSourceResponse
removeEventSourceResponse = RemoveEventSourceResponse

instance ToPath RemoveEventSource where
    toPath RemoveEventSource{..} = mconcat
        [ "/2014-11-13/event-source-mappings/"
        , toText _resUUID
        ]

instance ToQuery RemoveEventSource where
    toQuery = const mempty

instance ToHeaders RemoveEventSource

instance ToJSON RemoveEventSource where
    toJSON = const (toJSON Empty)

instance AWSRequest RemoveEventSource where
    type Sv RemoveEventSource = Lambda
    type Rs RemoveEventSource = RemoveEventSourceResponse

    request  = delete
    response = nullResponse RemoveEventSourceResponse


Some kind of operator / class to check the types whether to continue?
