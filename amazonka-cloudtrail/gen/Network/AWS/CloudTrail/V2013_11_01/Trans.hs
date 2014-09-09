{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | CloudTrail is a web service that records AWS API calls for your AWS account
-- and delivers log files to an Amazon S3 bucket. The recorded information
-- includes the identity of the user, the start time of the AWS API call, the
-- source IP address, the request parameters, and the response elements
-- returned by the service.
module Network.AWS.CloudTrail.V2013_11_01.Trans
    (
    -- * CreateTrail
      createTrail
    -- * DeleteTrail
    , deleteTrail
    -- * DescribeTrails
    , describeTrails
    -- * GetTrailStatus
    , getTrailStatus
    -- * StartLogging
    , startLogging
    -- * StopLogging
    , stopLogging
    -- * UpdateTrail
    , updateTrail

    -- * Re-exported
    , module Network.AWS.CloudTrail.V2013_11_01
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.CloudTrail.V2013_11_01

-- | From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.CreateTrail'
createTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'ctName'
            -> Text -- ^ 'ctS3BucketName'
            -> State CreateTrail a
            -> m CreateTrailResponse
createTrail p1 p2 s =
    send $ (mkCreateTrail p1 p2) &~ s

-- | Deletes a trail.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.DeleteTrail'
deleteTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'dtName'
            -> State DeleteTrail a
            -> m DeleteTrailResponse
deleteTrail p1 s =
    send $ (mkDeleteTrail p1) &~ s

-- | Retrieves settings for the trail associated with the current region for
-- your account.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.DescribeTrails'
describeTrails :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => State DescribeTrails a
               -> m DescribeTrailsResponse
describeTrails s =
    send $ (mkDescribeTrails) &~ s

-- | Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus'
getTrailStatus :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'gtsName'
               -> State GetTrailStatus a
               -> m GetTrailStatusResponse
getTrailStatus p1 s =
    send $ (mkGetTrailStatus p1) &~ s

-- | Starts the recording of AWS API calls and log file delivery for a trail.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.StartLogging'
startLogging :: ( MonadCatch m
                , MonadResource m
                , MonadError Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'slName'
             -> State StartLogging a
             -> m StartLoggingResponse
startLogging p1 s =
    send $ (mkStartLogging p1) &~ s

-- | Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.StopLogging'
stopLogging :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'sl1Name'
            -> State StopLogging a
            -> m StopLoggingResponse
stopLogging p1 s =
    send $ (mkStopLogging p1) &~ s

-- | From the command line, use update-subscription. Updates the settings that
-- specify delivery of log files. Changes to a trail do not require stopping
-- the CloudTrail service. Use this action to designate an existing bucket for
-- log delivery. If the existing bucket has previously been a target for
-- CloudTrail log files, an IAM policy exists for the bucket.
--
-- See: 'Network.AWS.CloudTrail.V2013_11_01.UpdateTrail'
updateTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'utName'
            -> State UpdateTrail a
            -> m UpdateTrailResponse
updateTrail p1 s =
    send $ (mkUpdateTrail p1) &~ s
