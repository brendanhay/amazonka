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

-- Module      : Network.AWS.CloudFormation.SignalResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sends a signal to the specified resource with a success or failure status.
-- You can use the SignalResource API in conjunction with a creation policy or
-- update policy. AWS CloudFormation doesn't proceed with a stack creation or
-- update until resources receive the required number of signals or the timeout
-- period is exceeded. The SignalResource API is useful in cases where you want
-- to send signals from anywhere other than an Amazon EC2 instance.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SignalResource.html>
module Network.AWS.CloudFormation.SignalResource
    (
    -- * Request
      SignalResource
    -- ** Request constructor
    , signalResource
    -- ** Request lenses
    , srLogicalResourceId
    , srStackName
    , srStatus
    , srUniqueId

    -- * Response
    , SignalResourceResponse
    -- ** Response constructor
    , signalResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data SignalResource = SignalResource
    { _srLogicalResourceId :: Text
    , _srStackName         :: Text
    , _srStatus            :: ResourceSignalStatus
    , _srUniqueId          :: Text
    } deriving (Eq, Show)

-- | 'SignalResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srLogicalResourceId' @::@ 'Text'
--
-- * 'srStackName' @::@ 'Text'
--
-- * 'srStatus' @::@ 'ResourceSignalStatus'
--
-- * 'srUniqueId' @::@ 'Text'
--
signalResource :: Text -- ^ 'srStackName'
               -> Text -- ^ 'srLogicalResourceId'
               -> Text -- ^ 'srUniqueId'
               -> ResourceSignalStatus -- ^ 'srStatus'
               -> SignalResource
signalResource p1 p2 p3 p4 = SignalResource
    { _srStackName         = p1
    , _srLogicalResourceId = p2
    , _srUniqueId          = p3
    , _srStatus            = p4
    }

-- | The logical ID of the resource that you want to signal. The logical ID is the
-- name of the resource that given in the template.
--
srLogicalResourceId :: Lens' SignalResource Text
srLogicalResourceId =
    lens _srLogicalResourceId (\s a -> s { _srLogicalResourceId = a })

-- | The stack name or ID that includes the resource that you want to signal.
--
srStackName :: Lens' SignalResource Text
srStackName = lens _srStackName (\s a -> s { _srStackName = a })

-- | The status of the signal, which is either success or failure. A failure
-- signal causes AWS CloudFormation to immediately fail the stack creation or
-- update.
--
srStatus :: Lens' SignalResource ResourceSignalStatus
srStatus = lens _srStatus (\s a -> s { _srStatus = a })

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto
-- Scaling groups, specify the instance ID that you are signaling as the unique
-- ID. If you send multiple signals to a single resource (such as signaling a
-- wait condition), each signal requires a different unique ID.
--
srUniqueId :: Lens' SignalResource Text
srUniqueId = lens _srUniqueId (\s a -> s { _srUniqueId = a })

data SignalResourceResponse = SignalResourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SignalResourceResponse' constructor.
signalResourceResponse :: SignalResourceResponse
signalResourceResponse = SignalResourceResponse

instance ToPath SignalResource where
    toPath = const "/"

instance ToQuery SignalResource where
    toQuery SignalResource{..} = mconcat
        [ "LogicalResourceId" =? _srLogicalResourceId
        , "StackName"         =? _srStackName
        , "Status"            =? _srStatus
        , "UniqueId"          =? _srUniqueId
        ]

instance ToHeaders SignalResource

instance AWSRequest SignalResource where
    type Sv SignalResource = CloudFormation
    type Rs SignalResource = SignalResourceResponse

    request  = post "SignalResource"
    response = nullResponse SignalResourceResponse
