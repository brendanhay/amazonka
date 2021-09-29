{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.QLDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestUpdateLedgerPermissionsMode $
--             newUpdateLedgerPermissionsMode
--
--         , requestUpdateLedger $
--             newUpdateLedger
--
--         , requestDeleteLedger $
--             newDeleteLedger
--
--         , requestListLedgers $
--             newListLedgers
--
--         , requestCreateLedger $
--             newCreateLedger
--
--         , requestExportJournalToS $
--             newExportJournalToS
--
--         , requestCancelJournalKinesisStream $
--             newCancelJournalKinesisStream
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeLedger $
--             newDescribeLedger
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeJournalS3Export $
--             newDescribeJournalS3Export
--
--         , requestGetRevision $
--             newGetRevision
--
--         , requestListJournalS3ExportsForLedger $
--             newListJournalS3ExportsForLedger
--
--         , requestListJournalKinesisStreamsForLedger $
--             newListJournalKinesisStreamsForLedger
--
--         , requestGetBlock $
--             newGetBlock
--
--         , requestListJournalS3Exports $
--             newListJournalS3Exports
--
--         , requestStreamJournalToKinesis $
--             newStreamJournalToKinesis
--
--         , requestGetDigest $
--             newGetDigest
--
--         , requestDescribeJournalKinesisStream $
--             newDescribeJournalKinesisStream
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseUpdateLedgerPermissionsMode $
--             newUpdateLedgerPermissionsModeResponse
--
--         , responseUpdateLedger $
--             newUpdateLedgerResponse
--
--         , responseDeleteLedger $
--             newDeleteLedgerResponse
--
--         , responseListLedgers $
--             newListLedgersResponse
--
--         , responseCreateLedger $
--             newCreateLedgerResponse
--
--         , responseExportJournalToS $
--             newExportJournalToSResponse
--
--         , responseCancelJournalKinesisStream $
--             newCancelJournalKinesisStreamResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeLedger $
--             newDescribeLedgerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeJournalS3Export $
--             newDescribeJournalS3ExportResponse
--
--         , responseGetRevision $
--             newGetRevisionResponse
--
--         , responseListJournalS3ExportsForLedger $
--             newListJournalS3ExportsForLedgerResponse
--
--         , responseListJournalKinesisStreamsForLedger $
--             newListJournalKinesisStreamsForLedgerResponse
--
--         , responseGetBlock $
--             newGetBlockResponse
--
--         , responseListJournalS3Exports $
--             newListJournalS3ExportsResponse
--
--         , responseStreamJournalToKinesis $
--             newStreamJournalToKinesisResponse
--
--         , responseGetDigest $
--             newGetDigestResponse
--
--         , responseDescribeJournalKinesisStream $
--             newDescribeJournalKinesisStreamResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestUpdateLedgerPermissionsMode :: UpdateLedgerPermissionsMode -> TestTree
requestUpdateLedgerPermissionsMode =
  req
    "UpdateLedgerPermissionsMode"
    "fixture/UpdateLedgerPermissionsMode.yaml"

requestUpdateLedger :: UpdateLedger -> TestTree
requestUpdateLedger =
  req
    "UpdateLedger"
    "fixture/UpdateLedger.yaml"

requestDeleteLedger :: DeleteLedger -> TestTree
requestDeleteLedger =
  req
    "DeleteLedger"
    "fixture/DeleteLedger.yaml"

requestListLedgers :: ListLedgers -> TestTree
requestListLedgers =
  req
    "ListLedgers"
    "fixture/ListLedgers.yaml"

requestCreateLedger :: CreateLedger -> TestTree
requestCreateLedger =
  req
    "CreateLedger"
    "fixture/CreateLedger.yaml"

requestExportJournalToS :: ExportJournalToS -> TestTree
requestExportJournalToS =
  req
    "ExportJournalToS"
    "fixture/ExportJournalToS.yaml"

requestCancelJournalKinesisStream :: CancelJournalKinesisStream -> TestTree
requestCancelJournalKinesisStream =
  req
    "CancelJournalKinesisStream"
    "fixture/CancelJournalKinesisStream.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeLedger :: DescribeLedger -> TestTree
requestDescribeLedger =
  req
    "DescribeLedger"
    "fixture/DescribeLedger.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeJournalS3Export :: DescribeJournalS3Export -> TestTree
requestDescribeJournalS3Export =
  req
    "DescribeJournalS3Export"
    "fixture/DescribeJournalS3Export.yaml"

requestGetRevision :: GetRevision -> TestTree
requestGetRevision =
  req
    "GetRevision"
    "fixture/GetRevision.yaml"

requestListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedger -> TestTree
requestListJournalS3ExportsForLedger =
  req
    "ListJournalS3ExportsForLedger"
    "fixture/ListJournalS3ExportsForLedger.yaml"

requestListJournalKinesisStreamsForLedger :: ListJournalKinesisStreamsForLedger -> TestTree
requestListJournalKinesisStreamsForLedger =
  req
    "ListJournalKinesisStreamsForLedger"
    "fixture/ListJournalKinesisStreamsForLedger.yaml"

requestGetBlock :: GetBlock -> TestTree
requestGetBlock =
  req
    "GetBlock"
    "fixture/GetBlock.yaml"

requestListJournalS3Exports :: ListJournalS3Exports -> TestTree
requestListJournalS3Exports =
  req
    "ListJournalS3Exports"
    "fixture/ListJournalS3Exports.yaml"

requestStreamJournalToKinesis :: StreamJournalToKinesis -> TestTree
requestStreamJournalToKinesis =
  req
    "StreamJournalToKinesis"
    "fixture/StreamJournalToKinesis.yaml"

requestGetDigest :: GetDigest -> TestTree
requestGetDigest =
  req
    "GetDigest"
    "fixture/GetDigest.yaml"

requestDescribeJournalKinesisStream :: DescribeJournalKinesisStream -> TestTree
requestDescribeJournalKinesisStream =
  req
    "DescribeJournalKinesisStream"
    "fixture/DescribeJournalKinesisStream.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseUpdateLedgerPermissionsMode :: UpdateLedgerPermissionsModeResponse -> TestTree
responseUpdateLedgerPermissionsMode =
  res
    "UpdateLedgerPermissionsModeResponse"
    "fixture/UpdateLedgerPermissionsModeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLedgerPermissionsMode)

responseUpdateLedger :: UpdateLedgerResponse -> TestTree
responseUpdateLedger =
  res
    "UpdateLedgerResponse"
    "fixture/UpdateLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLedger)

responseDeleteLedger :: DeleteLedgerResponse -> TestTree
responseDeleteLedger =
  res
    "DeleteLedgerResponse"
    "fixture/DeleteLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLedger)

responseListLedgers :: ListLedgersResponse -> TestTree
responseListLedgers =
  res
    "ListLedgersResponse"
    "fixture/ListLedgersResponse.proto"
    defaultService
    (Proxy :: Proxy ListLedgers)

responseCreateLedger :: CreateLedgerResponse -> TestTree
responseCreateLedger =
  res
    "CreateLedgerResponse"
    "fixture/CreateLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLedger)

responseExportJournalToS :: ExportJournalToSResponse -> TestTree
responseExportJournalToS =
  res
    "ExportJournalToSResponse"
    "fixture/ExportJournalToSResponse.proto"
    defaultService
    (Proxy :: Proxy ExportJournalToS)

responseCancelJournalKinesisStream :: CancelJournalKinesisStreamResponse -> TestTree
responseCancelJournalKinesisStream =
  res
    "CancelJournalKinesisStreamResponse"
    "fixture/CancelJournalKinesisStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJournalKinesisStream)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeLedger :: DescribeLedgerResponse -> TestTree
responseDescribeLedger =
  res
    "DescribeLedgerResponse"
    "fixture/DescribeLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLedger)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeJournalS3Export :: DescribeJournalS3ExportResponse -> TestTree
responseDescribeJournalS3Export =
  res
    "DescribeJournalS3ExportResponse"
    "fixture/DescribeJournalS3ExportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJournalS3Export)

responseGetRevision :: GetRevisionResponse -> TestTree
responseGetRevision =
  res
    "GetRevisionResponse"
    "fixture/GetRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy GetRevision)

responseListJournalS3ExportsForLedger :: ListJournalS3ExportsForLedgerResponse -> TestTree
responseListJournalS3ExportsForLedger =
  res
    "ListJournalS3ExportsForLedgerResponse"
    "fixture/ListJournalS3ExportsForLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy ListJournalS3ExportsForLedger)

responseListJournalKinesisStreamsForLedger :: ListJournalKinesisStreamsForLedgerResponse -> TestTree
responseListJournalKinesisStreamsForLedger =
  res
    "ListJournalKinesisStreamsForLedgerResponse"
    "fixture/ListJournalKinesisStreamsForLedgerResponse.proto"
    defaultService
    (Proxy :: Proxy ListJournalKinesisStreamsForLedger)

responseGetBlock :: GetBlockResponse -> TestTree
responseGetBlock =
  res
    "GetBlockResponse"
    "fixture/GetBlockResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlock)

responseListJournalS3Exports :: ListJournalS3ExportsResponse -> TestTree
responseListJournalS3Exports =
  res
    "ListJournalS3ExportsResponse"
    "fixture/ListJournalS3ExportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJournalS3Exports)

responseStreamJournalToKinesis :: StreamJournalToKinesisResponse -> TestTree
responseStreamJournalToKinesis =
  res
    "StreamJournalToKinesisResponse"
    "fixture/StreamJournalToKinesisResponse.proto"
    defaultService
    (Proxy :: Proxy StreamJournalToKinesis)

responseGetDigest :: GetDigestResponse -> TestTree
responseGetDigest =
  res
    "GetDigestResponse"
    "fixture/GetDigestResponse.proto"
    defaultService
    (Proxy :: Proxy GetDigest)

responseDescribeJournalKinesisStream :: DescribeJournalKinesisStreamResponse -> TestTree
responseDescribeJournalKinesisStream =
  res
    "DescribeJournalKinesisStreamResponse"
    "fixture/DescribeJournalKinesisStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeJournalKinesisStream)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
