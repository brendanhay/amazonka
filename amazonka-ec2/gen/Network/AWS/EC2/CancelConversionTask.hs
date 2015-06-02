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

-- Module      : Network.AWS.EC2.CancelConversionTask
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

-- | Cancels an active conversion task. The task can be the import of an instance
-- or volume. The action removes all artifacts of the conversion, including a
-- partially uploaded volume or instance. If the conversion is complete or is in
-- the process of transferring the final disk image, the command fails and
-- returns an exception.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import YourVirtual Machine to Amazon EC2> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelConversionTask.html>
module Network.AWS.EC2.CancelConversionTask
    (
    -- * Request
      CancelConversionTask
    -- ** Request constructor
    , cancelConversionTask
    -- ** Request lenses
    , cctConversionTaskId
    , cctDryRun
    , cctReasonMessage

    -- * Response
    , CancelConversionTaskResponse
    -- ** Response constructor
    , cancelConversionTaskResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelConversionTask = CancelConversionTask
    { _cctConversionTaskId :: Text
    , _cctDryRun           :: Maybe Bool
    , _cctReasonMessage    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelConversionTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cctConversionTaskId' @::@ 'Text'
--
-- * 'cctDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cctReasonMessage' @::@ 'Maybe' 'Text'
--
cancelConversionTask :: Text -- ^ 'cctConversionTaskId'
                     -> CancelConversionTask
cancelConversionTask p1 = CancelConversionTask
    { _cctConversionTaskId = p1
    , _cctDryRun           = Nothing
    , _cctReasonMessage    = Nothing
    }

-- | The ID of the conversion task.
cctConversionTaskId :: Lens' CancelConversionTask Text
cctConversionTaskId =
    lens _cctConversionTaskId (\s a -> s { _cctConversionTaskId = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
cctDryRun :: Lens' CancelConversionTask (Maybe Bool)
cctDryRun = lens _cctDryRun (\s a -> s { _cctDryRun = a })

-- | The reason for canceling the conversion task.
cctReasonMessage :: Lens' CancelConversionTask (Maybe Text)
cctReasonMessage = lens _cctReasonMessage (\s a -> s { _cctReasonMessage = a })

data CancelConversionTaskResponse = CancelConversionTaskResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CancelConversionTaskResponse' constructor.
cancelConversionTaskResponse :: CancelConversionTaskResponse
cancelConversionTaskResponse = CancelConversionTaskResponse

instance ToPath CancelConversionTask where
    toPath = const "/"

instance ToQuery CancelConversionTask where
    toQuery CancelConversionTask{..} = mconcat
        [ "ConversionTaskId" =? _cctConversionTaskId
        , "DryRun"           =? _cctDryRun
        , "ReasonMessage"    =? _cctReasonMessage
        ]

instance ToHeaders CancelConversionTask

instance AWSRequest CancelConversionTask where
    type Sv CancelConversionTask = EC2
    type Rs CancelConversionTask = CancelConversionTaskResponse

    request  = post "CancelConversionTask"
    response = nullResponse CancelConversionTaskResponse
