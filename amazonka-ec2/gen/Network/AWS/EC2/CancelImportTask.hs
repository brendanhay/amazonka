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

-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Cancels an in-process import virtual machine or import snapshot task.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelImportTask.html>
module Network.AWS.EC2.CancelImportTask
    (
    -- * Request
      CancelImportTask
    -- ** Request constructor
    , cancelImportTask
    -- ** Request lenses
    , citCancelReason
    , citDryRun
    , citImportTaskId

    -- * Response
    , CancelImportTaskResponse
    -- ** Response constructor
    , cancelImportTaskResponse
    -- ** Response lenses
    , citrImportTaskId
    , citrPreviousState
    , citrState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelImportTask = CancelImportTask
    { _citCancelReason :: Maybe Text
    , _citDryRun       :: Maybe Bool
    , _citImportTaskId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelImportTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'citCancelReason' @::@ 'Maybe' 'Text'
--
-- * 'citDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'citImportTaskId' @::@ 'Maybe' 'Text'
--
cancelImportTask :: CancelImportTask
cancelImportTask = CancelImportTask
    { _citDryRun       = Nothing
    , _citImportTaskId = Nothing
    , _citCancelReason = Nothing
    }

-- | The reason for canceling the task.
citCancelReason :: Lens' CancelImportTask (Maybe Text)
citCancelReason = lens _citCancelReason (\s a -> s { _citCancelReason = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
citDryRun :: Lens' CancelImportTask (Maybe Bool)
citDryRun = lens _citDryRun (\s a -> s { _citDryRun = a })

-- | The ID of the import image or import snapshot task to be canceled.
citImportTaskId :: Lens' CancelImportTask (Maybe Text)
citImportTaskId = lens _citImportTaskId (\s a -> s { _citImportTaskId = a })

data CancelImportTaskResponse = CancelImportTaskResponse
    { _citrImportTaskId  :: Maybe Text
    , _citrPreviousState :: Maybe Text
    , _citrState         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelImportTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'citrImportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'citrPreviousState' @::@ 'Maybe' 'Text'
--
-- * 'citrState' @::@ 'Maybe' 'Text'
--
cancelImportTaskResponse :: CancelImportTaskResponse
cancelImportTaskResponse = CancelImportTaskResponse
    { _citrImportTaskId  = Nothing
    , _citrState         = Nothing
    , _citrPreviousState = Nothing
    }

-- | The ID of the task being canceled.
citrImportTaskId :: Lens' CancelImportTaskResponse (Maybe Text)
citrImportTaskId = lens _citrImportTaskId (\s a -> s { _citrImportTaskId = a })

-- | The current state of the task being canceled.
citrPreviousState :: Lens' CancelImportTaskResponse (Maybe Text)
citrPreviousState =
    lens _citrPreviousState (\s a -> s { _citrPreviousState = a })

-- | The current state of the task being canceled.
citrState :: Lens' CancelImportTaskResponse (Maybe Text)
citrState = lens _citrState (\s a -> s { _citrState = a })

instance ToPath CancelImportTask where
    toPath = const "/"

instance ToQuery CancelImportTask where
    toQuery CancelImportTask{..} = mconcat
        [ "CancelReason" =? _citCancelReason
        , "DryRun"       =? _citDryRun
        , "ImportTaskId" =? _citImportTaskId
        ]

instance ToHeaders CancelImportTask

instance AWSRequest CancelImportTask where
    type Sv CancelImportTask = EC2
    type Rs CancelImportTask = CancelImportTaskResponse

    request  = post "CancelImportTask"
    response = xmlResponse

instance FromXML CancelImportTaskResponse where
    parseXML x = CancelImportTaskResponse
        <$> x .@? "importTaskId"
        <*> x .@? "previousState"
        <*> x .@? "state"
