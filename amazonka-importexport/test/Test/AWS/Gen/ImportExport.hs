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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ImportExport

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ cancelJobTest $
--             cancelJob
--
--         , createJobTest $
--             createJob
--
--         , getShippingLabelTest $
--             getShippingLabel
--
--         , getStatusTest $
--             getStatus
--
--         , listJobsTest $
--             listJobs
--
--         , updateJobTest $
--             updateJob
--
--           ]

--     , testGroup "response"
--         [ cancelJobResponseTest $
--             cancelJobResponse
--
--         , createJobResponseTest $
--             createJobResponse
--
--         , getShippingLabelResponseTest $
--             getShippingLabelResponse
--
--         , getStatusResponseTest $
--             getStatusResponse
--
--         , listJobsResponseTest $
--             listJobsResponse
--
--         , updateJobResponseTest $
--             updateJobResponse
--
--           ]
--     ]

-- Requests

cancelJobTest :: CancelJob -> TestTree
cancelJobTest = undefined

createJobTest :: CreateJob -> TestTree
createJobTest = undefined

getShippingLabelTest :: GetShippingLabel -> TestTree
getShippingLabelTest = undefined

getStatusTest :: GetStatus -> TestTree
getStatusTest = undefined

listJobsTest :: ListJobs -> TestTree
listJobsTest = undefined

updateJobTest :: UpdateJob -> TestTree
updateJobTest = undefined

-- Responses

cancelJobResponseTest :: CancelJobResponse -> TestTree
cancelJobResponseTest = resp
    "cancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)

createJobResponseTest :: CreateJobResponse -> TestTree
createJobResponseTest = resp
    "createJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

getShippingLabelResponseTest :: GetShippingLabelResponse -> TestTree
getShippingLabelResponseTest = resp
    "getShippingLabelResponse"
    "fixture/GetShippingLabelResponse"
    (Proxy :: Proxy GetShippingLabel)

getStatusResponseTest :: GetStatusResponse -> TestTree
getStatusResponseTest = resp
    "getStatusResponse"
    "fixture/GetStatusResponse"
    (Proxy :: Proxy GetStatus)

listJobsResponseTest :: ListJobsResponse -> TestTree
listJobsResponseTest = resp
    "listJobsResponse"
    "fixture/ListJobsResponse"
    (Proxy :: Proxy ListJobs)

updateJobResponseTest :: UpdateJobResponse -> TestTree
updateJobResponseTest = resp
    "updateJobResponse"
    "fixture/UpdateJobResponse"
    (Proxy :: Proxy UpdateJob)
