-- Module      : Test.AWS.Gen.ImportExport
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ImportExport where

import           Data.Proxy
import           Network.AWS.ImportExport
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getShippingLabelTest $
--             getShippingLabel
--
--         , createJobTest $
--             createJob
--
--         , listJobsTest $
--             listJobs
--
--         , updateJobTest $
--             updateJob
--
--         , getStatusTest $
--             getStatus
--
--         , cancelJobTest $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ getShippingLabelResponseTest $
--             getShippingLabelResponse
--
--         , createJobResponseTest $
--             createJobResponse
--
--         , listJobsResponseTest $
--             listJobsResponse
--
--         , updateJobResponseTest $
--             updateJobResponse
--
--         , getStatusResponseTest $
--             getStatusResponse
--
--         , cancelJobResponseTest $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

getShippingLabelTest :: GetShippingLabel -> TestTree
getShippingLabelTest = undefined

createJobTest :: CreateJob -> TestTree
createJobTest = undefined

listJobsTest :: ListJobs -> TestTree
listJobsTest = undefined

updateJobTest :: UpdateJob -> TestTree
updateJobTest = undefined

getStatusTest :: GetStatus -> TestTree
getStatusTest = undefined

cancelJobTest :: CancelJob -> TestTree
cancelJobTest = undefined

-- Responses

getShippingLabelResponseTest :: GetShippingLabelResponse -> TestTree
getShippingLabelResponseTest = resp
    "GetShippingLabelResponse"
    "fixture/ImportExport/GetShippingLabelResponse"
    (Proxy :: Proxy GetShippingLabel)

createJobResponseTest :: CreateJobResponse -> TestTree
createJobResponseTest = resp
    "CreateJobResponse"
    "fixture/ImportExport/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

listJobsResponseTest :: ListJobsResponse -> TestTree
listJobsResponseTest = resp
    "ListJobsResponse"
    "fixture/ImportExport/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

updateJobResponseTest :: UpdateJobResponse -> TestTree
updateJobResponseTest = resp
    "UpdateJobResponse"
    "fixture/ImportExport/UpdateJobResponse"
    (Proxy :: Proxy UpdateJob)

getStatusResponseTest :: GetStatusResponse -> TestTree
getStatusResponseTest = resp
    "GetStatusResponse"
    "fixture/ImportExport/GetStatusResponse"
    (Proxy :: Proxy GetStatus)

cancelJobResponseTest :: CancelJobResponse -> TestTree
cancelJobResponseTest = resp
    "CancelJobResponse"
    "fixture/ImportExport/CancelJobResponse"
    (Proxy :: Proxy CancelJob)
