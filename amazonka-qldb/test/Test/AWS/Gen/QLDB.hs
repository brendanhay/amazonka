{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.QLDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.QLDB where

import Data.Proxy
import Network.AWS.QLDB
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.QLDB.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateLedger $
--             updateLedger
--
--         , requestDeleteLedger $
--             deleteLedger
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetRevision $
--             getRevision
--
--         , requestDescribeLedger $
--             describeLedger
--
--         , requestExportJournalToS3 $
--             exportJournalToS3
--
--         , requestCreateLedger $
--             createLedger
--
--         , requestListLedgers $
--             listLedgers
--
--         , requestListJournalS3Exports $
--             listJournalS3Exports
--
--         , requestGetBlock $
--             getBlock
--
--         , requestListJournalS3ExportsForLedger $
--             listJournalS3ExportsForLedger
--
--         , requestDescribeJournalS3Export $
--             describeJournalS3Export
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetDigest $
--             getDigest
--
--           ]

--     , testGroup "response"
--         [ responseUpdateLedger $
--             updateLedgerResponse
--
--         , responseDeleteLedger $
--             deleteLedgerResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetRevision $
--             getRevisionResponse
--
--         , responseDescribeLedger $
--             describeLedgerResponse
--
--         , responseExportJournalToS3 $
--             exportJournalToS3Response
--
--         , responseCreateLedger $
--             createLedgerResponse
--
--         , responseListLedgers $
--             listLedgersResponse
--
--         , responseListJournalS3Exports $
--             listJournalS3ExportsResponse
--
--         , responseGetBlock $
--             getBlockResponse
--
--         , responseListJournalS3ExportsForLedger $
--             listJournalS3ExportsForLedgerResponse
--
--         , responseDescribeJournalS3Export $
--             describeJournalS3ExportResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetDigest $
--             getDigestResponse
--
--           ]
--     ]

-- Requests

requestUpdateLedger :: UpdateLedger -> TestTree
requestUpdateLedger = req
    "UpdateLedger"
    "fixture/UpdateLedger.yaml"

requestDeleteLedger :: DeleteLedger -> TestTree
requestDeleteLedger = req
    "DeleteLedger"
    "fixture/DeleteLedger.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetRevision :: GetRevision -> TestTree
requestGetRevision = req
    "GetRevision"
    "fixture/GetRevision.yaml"

requestDescribeLedger :: DescribeLedger -> TestTree
requestDescribeLedger = req
    "DescribeLedger"
    "fixture/DescribeLedger.yaml"

requestExportJournalToS3 :: ExportJournalToS3 -> TestTree
requestExportJournalToS3 = req
    "ExportJournalToS3"
    "fixture/ExportJournalToS3.yaml"

requestCreateLedger :: CreateLedger -> TestTree
requestCreateLedger = req
    "CreateLedger"
    "fixture/CreateLedger.yaml"

requestListLedgers :: ListLedgers -> TestTree
requestListLedgers = req
    "ListLedgers"
    "fixture/ListLedgers.yaml"

requestListJournalS3Exports :: ListJournalS3Exports -> TestTree
requestListJournalS3Exports = req
    "ListJournalS3Exports"
    "fixture/ListJournalS3Exports.yaml"

requestGetBlock :: GetBlock -> TestTree
requestGetBlock = req
    "GetBlock"
    "fixture/GetBlock.yaml"

requestListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedger -> TestTree
requestListJournalS3ExportsForLedger = req
    "ListJournalS3ExportsForLedger"
    "fixture/ListJournalS3ExportsForLedger.yaml"

requestDescribeJournalS3Export :: DescribeJournalS3Export -> TestTree
requestDescribeJournalS3Export = req
    "DescribeJournalS3Export"
    "fixture/DescribeJournalS3Export.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetDigest :: GetDigest -> TestTree
requestGetDigest = req
    "GetDigest"
    "fixture/GetDigest.yaml"

-- Responses

responseUpdateLedger :: UpdateLedgerResponse -> TestTree
responseUpdateLedger = res
    "UpdateLedgerResponse"
    "fixture/UpdateLedgerResponse.proto"
    qldb
    (Proxy :: Proxy UpdateLedger)

responseDeleteLedger :: DeleteLedgerResponse -> TestTree
responseDeleteLedger = res
    "DeleteLedgerResponse"
    "fixture/DeleteLedgerResponse.proto"
    qldb
    (Proxy :: Proxy DeleteLedger)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    qldb
    (Proxy :: Proxy ListTagsForResource)

responseGetRevision :: GetRevisionResponse -> TestTree
responseGetRevision = res
    "GetRevisionResponse"
    "fixture/GetRevisionResponse.proto"
    qldb
    (Proxy :: Proxy GetRevision)

responseDescribeLedger :: DescribeLedgerResponse -> TestTree
responseDescribeLedger = res
    "DescribeLedgerResponse"
    "fixture/DescribeLedgerResponse.proto"
    qldb
    (Proxy :: Proxy DescribeLedger)

responseExportJournalToS3 :: ExportJournalToS3Response -> TestTree
responseExportJournalToS3 = res
    "ExportJournalToS3Response"
    "fixture/ExportJournalToS3Response.proto"
    qldb
    (Proxy :: Proxy ExportJournalToS3)

responseCreateLedger :: CreateLedgerResponse -> TestTree
responseCreateLedger = res
    "CreateLedgerResponse"
    "fixture/CreateLedgerResponse.proto"
    qldb
    (Proxy :: Proxy CreateLedger)

responseListLedgers :: ListLedgersResponse -> TestTree
responseListLedgers = res
    "ListLedgersResponse"
    "fixture/ListLedgersResponse.proto"
    qldb
    (Proxy :: Proxy ListLedgers)

responseListJournalS3Exports :: ListJournalS3ExportsResponse -> TestTree
responseListJournalS3Exports = res
    "ListJournalS3ExportsResponse"
    "fixture/ListJournalS3ExportsResponse.proto"
    qldb
    (Proxy :: Proxy ListJournalS3Exports)

responseGetBlock :: GetBlockResponse -> TestTree
responseGetBlock = res
    "GetBlockResponse"
    "fixture/GetBlockResponse.proto"
    qldb
    (Proxy :: Proxy GetBlock)

responseListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedgerResponse -> TestTree
responseListJournalS3ExportsForLedger = res
    "ListJournalS3ExportsForLedgerResponse"
    "fixture/ListJournalS3ExportsForLedgerResponse.proto"
    qldb
    (Proxy :: Proxy ListJournalS3ExportsForLedger)

responseDescribeJournalS3Export :: DescribeJournalS3ExportResponse -> TestTree
responseDescribeJournalS3Export = res
    "DescribeJournalS3ExportResponse"
    "fixture/DescribeJournalS3ExportResponse.proto"
    qldb
    (Proxy :: Proxy DescribeJournalS3Export)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    qldb
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    qldb
    (Proxy :: Proxy UntagResource)

responseGetDigest :: GetDigestResponse -> TestTree
responseGetDigest = res
    "GetDigestResponse"
    "fixture/GetDigestResponse.proto"
    qldb
    (Proxy :: Proxy GetDigest)
