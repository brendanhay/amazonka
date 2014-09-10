{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudTrail.Monadic
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
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.CloudTrail" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CloudTrail
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CloudTrail.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.CloudTrail.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.CloudTrail.Monadic
    (
    -- * CreateTrail
    -- $CreateTrail
      createTrail
    , createTrailCatch

    -- * DeleteTrail
    -- $DeleteTrail
    , deleteTrail
    , deleteTrailCatch

    -- * DescribeTrails
    -- $DescribeTrails
    , describeTrails
    , describeTrailsCatch

    -- * GetTrailStatus
    -- $GetTrailStatus
    , getTrailStatus
    , getTrailStatusCatch

    -- * StartLogging
    -- $StartLogging
    , startLogging
    , startLoggingCatch

    -- * StopLogging
    -- $StopLogging
    , stopLogging
    , stopLoggingCatch

    -- * UpdateTrail
    -- $UpdateTrail
    , updateTrail
    , updateTrailCatch

    -- * Re-exported
    , module Network.AWS.CloudTrail

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudTrail

type ServiceEr = Er CloudTrail

-- $CreateTrail
-- From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
--
-- See: 'Network.AWS.CloudTrail'

createTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'ctName'
    -> Text -- ^ 'ctS3BucketName'
    -> State CreateTrail a
    -> m CreateTrailResponse
createTrail p1 p2 s =
    send $ (mkCreateTrail p1 p2) &~ s

createTrailCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ctName'
    -> Text -- ^ 'ctS3BucketName'
    -> State CreateTrail a
    -> m (Either ServiceEr CreateTrailResponse)
createTrailCatch p1 p2 s =
    sendCatch $ (mkCreateTrail p1 p2) &~ s

-- $DeleteTrail
-- Deletes a trail.
--
-- See: 'Network.AWS.CloudTrail'

deleteTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'dtName'
    -> State DeleteTrail a
    -> m DeleteTrailResponse
deleteTrail p1 s =
    send $ (mkDeleteTrail p1) &~ s

deleteTrailCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dtName'
    -> State DeleteTrail a
    -> m (Either ServiceEr DeleteTrailResponse)
deleteTrailCatch p1 s =
    sendCatch $ (mkDeleteTrail p1) &~ s

-- $DescribeTrails
-- Retrieves settings for the trail associated with the current region for
-- your account.
--
-- See: 'Network.AWS.CloudTrail'

describeTrails :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => State DescribeTrails a
    -> m DescribeTrailsResponse
describeTrails s =
    send (mkDescribeTrails &~ s)

describeTrailsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => State DescribeTrails a
    -> m (Either ServiceEr DescribeTrailsResponse)
describeTrailsCatch s =
    sendCatch (mkDescribeTrails &~ s)

-- $GetTrailStatus
-- Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail.
--
-- See: 'Network.AWS.CloudTrail'

getTrailStatus :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'gtsName'
    -> State GetTrailStatus a
    -> m GetTrailStatusResponse
getTrailStatus p1 s =
    send $ (mkGetTrailStatus p1) &~ s

getTrailStatusCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'gtsName'
    -> State GetTrailStatus a
    -> m (Either ServiceEr GetTrailStatusResponse)
getTrailStatusCatch p1 s =
    sendCatch $ (mkGetTrailStatus p1) &~ s

-- $StartLogging
-- Starts the recording of AWS API calls and log file delivery for a trail.
--
-- See: 'Network.AWS.CloudTrail'

startLogging :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'slName'
    -> State StartLogging a
    -> m StartLoggingResponse
startLogging p1 s =
    send $ (mkStartLogging p1) &~ s

startLoggingCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'slName'
    -> State StartLogging a
    -> m (Either ServiceEr StartLoggingResponse)
startLoggingCatch p1 s =
    sendCatch $ (mkStartLogging p1) &~ s

-- $StopLogging
-- Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording.
--
-- See: 'Network.AWS.CloudTrail'

stopLogging :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'sl1Name'
    -> State StopLogging a
    -> m StopLoggingResponse
stopLogging p1 s =
    send $ (mkStopLogging p1) &~ s

stopLoggingCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'sl1Name'
    -> State StopLogging a
    -> m (Either ServiceEr StopLoggingResponse)
stopLoggingCatch p1 s =
    sendCatch $ (mkStopLogging p1) &~ s

-- $UpdateTrail
-- From the command line, use update-subscription. Updates the settings that
-- specify delivery of log files. Changes to a trail do not require stopping
-- the CloudTrail service. Use this action to designate an existing bucket for
-- log delivery. If the existing bucket has previously been a target for
-- CloudTrail log files, an IAM policy exists for the bucket.
--
-- See: 'Network.AWS.CloudTrail'

updateTrail :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'utName'
    -> State UpdateTrail a
    -> m UpdateTrailResponse
updateTrail p1 s =
    send $ (mkUpdateTrail p1) &~ s

updateTrailCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'utName'
    -> State UpdateTrail a
    -> m (Either ServiceEr UpdateTrailResponse)
updateTrailCatch p1 s =
    sendCatch $ (mkUpdateTrail p1) &~ s
