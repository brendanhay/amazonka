{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.ImportExport" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.ImportExport
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.ImportExport.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.ImportExport.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using @return ()@:
-- operationName w x $ return ()
-- @
--
module Network.AWS.ImportExport.Monadic
    (
    -- * CancelJob
    -- $CancelJob
      cancelJob
    , cancelJobCatch

    -- * CreateJob
    -- $CreateJob
    , createJob
    , createJobCatch

    -- * GetStatus
    -- $GetStatus
    , getStatus
    , getStatusCatch

    -- * ListJobs
    -- $ListJobs
    , listJobs
    , listJobsCatch

    -- * UpdateJob
    -- $UpdateJob
    , updateJob
    , updateJobCatch

    -- * Re-exported
    , module Network.AWS.ImportExport

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.ImportExport

type ServiceEr = Er ImportExport

-- $CancelJob
-- This operation cancels a specified job. Only the job owner can cancel it.
-- The operation fails if the job has already started or is complete.
--
-- See: 'Network.AWS.ImportExport.CancelJob'

cancelJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'cjJobId'
    -> m CancelJobResponse
cancelJob p1 =
    send (mkCancelJob p1)

cancelJobCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cjJobId'
    -> m (Either ServiceEr CancelJobResponse)
cancelJobCatch p1 =
    sendCatch (mkCancelJob p1)

-- $CreateJob
-- This operation initiates the process of scheduling an upload or download of
-- your data. You include in the request a manifest that describes the data
-- transfer specifics. The response to the request includes a job ID, which
-- you can use in other operations, a signature that you use to identify your
-- storage device, and the address where you should ship your storage device.
--
-- See: 'Network.AWS.ImportExport.CreateJob'

createJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => JobType -- ^ 'cj1JobType'
    -> Text -- ^ 'cj1Manifest'
    -> Bool -- ^ 'cj1ValidateOnly'
    -> State CreateJob a
    -> m CreateJobResponse
createJob p1 p2 p4 s =
    send $ (mkCreateJob p1 p2 p4) &~ s

createJobCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => JobType -- ^ 'cj1JobType'
    -> Text -- ^ 'cj1Manifest'
    -> Bool -- ^ 'cj1ValidateOnly'
    -> State CreateJob a
    -> m (Either ServiceEr CreateJobResponse)
createJobCatch p1 p2 p4 s =
    sendCatch $ (mkCreateJob p1 p2 p4) &~ s

-- $GetStatus
-- This operation returns information about a job, including where the job is
-- in the processing pipeline, the status of the results, and the signature
-- value associated with the job. You can only return information about jobs
-- you own.
--
-- See: 'Network.AWS.ImportExport.GetStatus'

getStatus :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'gsJobId'
    -> m GetStatusResponse
getStatus p1 =
    send (mkGetStatus p1)

getStatusCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'gsJobId'
    -> m (Either ServiceEr GetStatusResponse)
getStatusCatch p1 =
    sendCatch (mkGetStatus p1)

-- $ListJobs
-- This operation returns the jobs associated with the requester. AWS
-- Import/Export lists the jobs in reverse chronological order based on the
-- date of creation. For example if Job Test1 was created 2009Dec30 and Test2
-- was created 2010Feb05, the ListJobs operation would return Test2 followed
-- by Test1.
--
-- See: 'Network.AWS.ImportExport.ListJobs'

listJobs :: ( MonadCatch m
            , MonadResource m
            , MonadError AWS.Error m
            , MonadReader Env m
            )
    => State ListJobs a
    -> Source m ListJobsResponse
listJobs s =
    paginate (mkListJobs &~ s)

listJobsCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env m
                 )
    => State ListJobs a
    -> Source m (Either ServiceEr ListJobsResponse)
listJobsCatch s =
    paginateCatch (mkListJobs &~ s)

-- $UpdateJob
-- You use this operation to change the parameters specified in the original
-- manifest file by supplying a new manifest file. The manifest file attached
-- to this request replaces the original manifest file. You can only use the
-- operation after a CreateJob request but before the data transfer starts and
-- you can only use it on jobs you own.
--
-- See: 'Network.AWS.ImportExport.UpdateJob'

updateJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'ujJobId'
    -> Text -- ^ 'ujManifest'
    -> JobType -- ^ 'ujJobType'
    -> Bool -- ^ 'ujValidateOnly'
    -> m UpdateJobResponse
updateJob p1 p2 p3 p4 =
    send (mkUpdateJob p1 p2 p3 p4)

updateJobCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'ujJobId'
    -> Text -- ^ 'ujManifest'
    -> JobType -- ^ 'ujJobType'
    -> Bool -- ^ 'ujValidateOnly'
    -> m (Either ServiceEr UpdateJobResponse)
updateJobCatch p1 p2 p3 p4 =
    sendCatch (mkUpdateJob p1 p2 p3 p4)
