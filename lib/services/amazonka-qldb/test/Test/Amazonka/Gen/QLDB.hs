{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.QLDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.QLDB where

import Amazonka.QLDB
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.QLDB.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelJournalKinesisStream $
--             newCancelJournalKinesisStream
--
--         , requestCreateLedger $
--             newCreateLedger
--
--         , requestDeleteLedger $
--             newDeleteLedger
--
--         , requestDescribeJournalKinesisStream $
--             newDescribeJournalKinesisStream
--
--         , requestDescribeJournalS3Export $
--             newDescribeJournalS3Export
--
--         , requestDescribeLedger $
--             newDescribeLedger
--
--         , requestExportJournalToS3 $
--             newExportJournalToS3
--
--         , requestGetBlock $
--             newGetBlock
--
--         , requestGetDigest $
--             newGetDigest
--
--         , requestGetRevision $
--             newGetRevision
--
--         , requestListJournalKinesisStreamsForLedger $
--             newListJournalKinesisStreamsForLedger
--
--         , requestListJournalS3Exports $
--             newListJournalS3Exports
--
--         , requestListJournalS3ExportsForLedger $
--             newListJournalS3ExportsForLedger
--
--         , requestListLedgers $
--             newListLedgers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStreamJournalToKinesis $
--             newStreamJournalToKinesis
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateLedger $
--             newUpdateLedger
--
--         , requestUpdateLedgerPermissionsMode $
--             newUpdateLedgerPermissionsMode
--
--           ]

--     , testGroup "response"
--         [ responseCancelJournalKinesisStream $
--             newCancelJournalKinesisStreamResponse
--
--         , responseCreateLedger $
--             newCreateLedgerResponse
--
--         , responseDeleteLedger $
--             newDeleteLedgerResponse
--
--         , responseDescribeJournalKinesisStream $
--             newDescribeJournalKinesisStreamResponse
--
--         , responseDescribeJournalS3Export $
--             newDescribeJournalS3ExportResponse
--
--         , responseDescribeLedger $
--             newDescribeLedgerResponse
--
--         , responseExportJournalToS3 $
--             newExportJournalToS3Response
--
--         , responseGetBlock $
--             newGetBlockResponse
--
--         , responseGetDigest $
--             newGetDigestResponse
--
--         , responseGetRevision $
--             newGetRevisionResponse
--
--         , responseListJournalKinesisStreamsForLedger $
--             newListJournalKinesisStreamsForLedgerResponse
--
--         , responseListJournalS3Exports $
--             newListJournalS3ExportsResponse
--
--         , responseListJournalS3ExportsForLedger $
--             newListJournalS3ExportsForLedgerResponse
--
--         , responseListLedgers $
--             newListLedgersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStreamJournalToKinesis $
--             newStreamJournalToKinesisResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateLedger $
--             newUpdateLedgerResponse
--
--         , responseUpdateLedgerPermissionsMode $
--             newUpdateLedgerPermissionsModeResponse
--
--           ]
--     ]

-- Requests

requestCancelJournalKinesisStream :: CancelJournalKinesisStream -> TestTree
requestCancelJournalKinesisStream =
  req
    "CancelJournalKinesisStream"
    "fixture/CancelJournalKinesisStream.yaml"

requestCreateLedger :: CreateLedger -> TestTree
requestCreateLedger =
  req
    "CreateLedger"
    "fixture/CreateLedger.yaml"

requestDeleteLedger :: DeleteLedger -> TestTree
requestDeleteLedger =
  req
    "DeleteLedger"
    "fixture/DeleteLedger.yaml"

requestDescribeJournalKinesisStream :: DescribeJournalKinesisStream -> TestTree
requestDescribeJournalKinesisStream =
  req
    "DescribeJournalKinesisStream"
    "fixture/DescribeJournalKinesisStream.yaml"

requestDescribeJournalS3Export :: DescribeJournalS3Export -> TestTree
requestDescribeJournalS3Export =
  req
    "DescribeJournalS3Export"
    "fixture/DescribeJournalS3Export.yaml"

requestDescribeLedger :: DescribeLedger -> TestTree
requestDescribeLedger =
  req
    "DescribeLedger"
    "fixture/DescribeLedger.yaml"

requestExportJournalToS3 :: ExportJournalToS3 -> TestTree
requestExportJournalToS3 =
  req
    "ExportJournalToS3"
    "fixture/ExportJournalToS3.yaml"

requestGetBlock :: GetBlock -> TestTree
requestGetBlock =
  req
    "GetBlock"
    "fixture/GetBlock.yaml"

requestGetDigest :: GetDigest -> TestTree
requestGetDigest =
  req
    "GetDigest"
    "fixture/GetDigest.yaml"

requestGetRevision :: GetRevision -> TestTree
requestGetRevision =
  req
    "GetRevision"
    "fixture/GetRevision.yaml"

requestListJournalKinesisStreamsForLedger :: ListJournalKinesisStreamsForLedger -> TestTree
requestListJournalKinesisStreamsForLedger =
  req
    "ListJournalKinesisStreamsForLedger"
    "fixture/ListJournalKinesisStreamsForLedger.yaml"

requestListJournalS3Exports :: ListJournalS3Exports -> TestTree
requestListJournalS3Exports =
  req
    "ListJournalS3Exports"
    "fixture/ListJournalS3Exports.yaml"

requestListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedger -> TestTree
requestListJournalS3ExportsForLedger =
  req
    "ListJournalS3ExportsForLedger"
    "fixture/ListJournalS3ExportsForLedger.yaml"

requestListLedgers :: ListLedgers -> TestTree
requestListLedgers =
  req
    "ListLedgers"
    "fixture/ListLedgers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStreamJournalToKinesis :: StreamJournalToKinesis -> TestTree
requestStreamJournalToKinesis =
  req
    "StreamJournalToKinesis"
    "fixture/StreamJournalToKinesis.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateLedger :: UpdateLedger -> TestTree
requestUpdateLedger =
  req
    "UpdateLedger"
    "fixture/UpdateLedger.yaml"

requestUpdateLedgerPermissionsMode :: UpdateLedgerPermissionsMode -> TestTree
requestUpdateLedgerPermissionsMode =
  req
    "UpdateLedgerPermissionsMode"
    "fixture/UpdateLedgerPermissionsMode.yaml"

-- Responses

responseCancelJournalKinesisStream :: CancelJournalKinesisStreamResponse -> TestTree
responseCancelJournalKinesisStream =
  res
    "CancelJournalKinesisStreamResponse"
    "fixture/CancelJournalKinesisStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJournalKinesisStream)

responseCreateLedger :: CreateLedgerResponse -> TestTree
responseCreateLedger =
  res
    "CreateLedgerResponse"
    "fixture/CreateLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLedger)

responseDeleteLedger :: DeleteLedgerResponse -> TestTree
responseDeleteLedger =
  res
    "DeleteLedgerResponse"
    "fixture/DeleteLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLedger)

responseDescribeJournalKinesisStream :: DescribeJournalKinesisStreamResponse -> TestTree
responseDescribeJournalKinesisStream =
  res
    "DescribeJournalKinesisStreamResponse"
    "fixture/DescribeJournalKinesisStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJournalKinesisStream)

responseDescribeJournalS3Export :: DescribeJournalS3ExportResponse -> TestTree
responseDescribeJournalS3Export =
  res
    "DescribeJournalS3ExportResponse"
    "fixture/DescribeJournalS3ExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeJournalS3Export)

responseDescribeLedger :: DescribeLedgerResponse -> TestTree
responseDescribeLedger =
  res
    "DescribeLedgerResponse"
    "fixture/DescribeLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLedger)

responseExportJournalToS3 :: ExportJournalToS3Response -> TestTree
responseExportJournalToS3 =
  res
    "ExportJournalToS3Response"
    "fixture/ExportJournalToS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportJournalToS3)

responseGetBlock :: GetBlockResponse -> TestTree
responseGetBlock =
  res
    "GetBlockResponse"
    "fixture/GetBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlock)

responseGetDigest :: GetDigestResponse -> TestTree
responseGetDigest =
  res
    "GetDigestResponse"
    "fixture/GetDigestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDigest)

responseGetRevision :: GetRevisionResponse -> TestTree
responseGetRevision =
  res
    "GetRevisionResponse"
    "fixture/GetRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRevision)

responseListJournalKinesisStreamsForLedger :: ListJournalKinesisStreamsForLedgerResponse -> TestTree
responseListJournalKinesisStreamsForLedger =
  res
    "ListJournalKinesisStreamsForLedgerResponse"
    "fixture/ListJournalKinesisStreamsForLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJournalKinesisStreamsForLedger)

responseListJournalS3Exports :: ListJournalS3ExportsResponse -> TestTree
responseListJournalS3Exports =
  res
    "ListJournalS3ExportsResponse"
    "fixture/ListJournalS3ExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJournalS3Exports)

responseListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedgerResponse -> TestTree
responseListJournalS3ExportsForLedger =
  res
    "ListJournalS3ExportsForLedgerResponse"
    "fixture/ListJournalS3ExportsForLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJournalS3ExportsForLedger)

responseListLedgers :: ListLedgersResponse -> TestTree
responseListLedgers =
  res
    "ListLedgersResponse"
    "fixture/ListLedgersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLedgers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStreamJournalToKinesis :: StreamJournalToKinesisResponse -> TestTree
responseStreamJournalToKinesis =
  res
    "StreamJournalToKinesisResponse"
    "fixture/StreamJournalToKinesisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StreamJournalToKinesis)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateLedger :: UpdateLedgerResponse -> TestTree
responseUpdateLedger =
  res
    "UpdateLedgerResponse"
    "fixture/UpdateLedgerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLedger)

responseUpdateLedgerPermissionsMode :: UpdateLedgerPermissionsModeResponse -> TestTree
responseUpdateLedgerPermissionsMode =
  res
    "UpdateLedgerPermissionsModeResponse"
    "fixture/UpdateLedgerPermissionsModeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLedgerPermissionsMode)
