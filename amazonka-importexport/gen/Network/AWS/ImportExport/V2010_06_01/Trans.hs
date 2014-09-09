{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Import/Export accelerates moving large amounts of data into and out of
-- AWS using portable storage devices for transport. AWS transfers your data
-- directly onto and off of storage devices using Amazon’s high-speed internal
-- network and bypassing the Internet. For significant data sets, AWS
-- Import/Export is often faster than Internet transfer and more cost
-- effective than upgrading your connectivity.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.ImportExport.V2010_06_01.Trans
    (
    -- * CancelJob
      cancelJob
    -- * CreateJob
    , createJob
    -- * GetStatus
    , getStatus
    -- * ListJobs
    , listJobs
    -- * UpdateJob
    , updateJob

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.ImportExport.V2010_06_01
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.ImportExport.V2010_06_01

-- | This operation cancels a specified job. Only the job owner can cancel it.
-- The operation fails if the job has already started or is complete.
--
-- See: 'Network.AWS.ImportExport.V2010_06_01.CancelJob'
cancelJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             , AWSRequest a
             )
          => Text -- ^ 'cjJobId'
          -> State CancelJob a
          -> m CancelJobResponse
cancelJob p1 s =
    send $ (mkCancelJob p1) &~ s

-- | This operation initiates the process of scheduling an upload or download of
-- your data. You include in the request a manifest that describes the data
-- transfer specifics. The response to the request includes a job ID, which
-- you can use in other operations, a signature that you use to identify your
-- storage device, and the address where you should ship your storage device.
--
-- See: 'Network.AWS.ImportExport.V2010_06_01.CreateJob'
createJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             , AWSRequest a
             )
          => JobType -- ^ 'cj1JobType'
          -> Text -- ^ 'cj1Manifest'
          -> Bool -- ^ 'cj1ValidateOnly'
          -> State CreateJob a
          -> m CreateJobResponse
createJob p1 p2 p4 s =
    send $ (mkCreateJob p1 p2 p4) &~ s

-- | This operation returns information about a job, including where the job is
-- in the processing pipeline, the status of the results, and the signature
-- value associated with the job. You can only return information about jobs
-- you own.
--
-- See: 'Network.AWS.ImportExport.V2010_06_01.GetStatus'
getStatus :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             , AWSRequest a
             )
          => Text -- ^ 'gsJobId'
          -> State GetStatus a
          -> m GetStatusResponse
getStatus p1 s =
    send $ (mkGetStatus p1) &~ s

-- | This operation returns the jobs associated with the requester. AWS
-- Import/Export lists the jobs in reverse chronological order based on the
-- date of creation. For example if Job Test1 was created 2009Dec30 and Test2
-- was created 2010Feb05, the ListJobs operation would return Test2 followed
-- by Test1.
--
-- See: 'Network.AWS.ImportExport.V2010_06_01.ListJobs'
listJobs :: ( MonadCatch m
            , MonadResource m
            , MonadError AWS.Error m
            , MonadReader Env (ResumableSource m)
            , AWSPager a
            )
         => State ListJobs a
         -> ResumableSource m ListJobsResponse
listJobs s =
    paginate (mkListJobs &~ s)

-- | You use this operation to change the parameters specified in the original
-- manifest file by supplying a new manifest file. The manifest file attached
-- to this request replaces the original manifest file. You can only use the
-- operation after a CreateJob request but before the data transfer starts and
-- you can only use it on jobs you own.
--
-- See: 'Network.AWS.ImportExport.V2010_06_01.UpdateJob'
updateJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             , AWSRequest a
             )
          => Text -- ^ 'ujJobId'
          -> Text -- ^ 'ujManifest'
          -> JobType -- ^ 'ujJobType'
          -> Bool -- ^ 'ujValidateOnly'
          -> State UpdateJob a
          -> m UpdateJobResponse
updateJob p1 p2 p3 p4 s =
    send $ (mkUpdateJob p1 p2 p3 p4) &~ s
