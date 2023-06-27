{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PaymentCryptography
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PaymentCryptography where

import Amazonka.PaymentCryptography
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PaymentCryptography.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateKey $
--             newCreateKey
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDeleteKey $
--             newDeleteKey
--
--         , requestExportKey $
--             newExportKey
--
--         , requestGetAlias $
--             newGetAlias
--
--         , requestGetKey $
--             newGetKey
--
--         , requestGetParametersForExport $
--             newGetParametersForExport
--
--         , requestGetParametersForImport $
--             newGetParametersForImport
--
--         , requestGetPublicKeyCertificate $
--             newGetPublicKeyCertificate
--
--         , requestImportKey $
--             newImportKey
--
--         , requestListAliases $
--             newListAliases
--
--         , requestListKeys $
--             newListKeys
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRestoreKey $
--             newRestoreKey
--
--         , requestStartKeyUsage $
--             newStartKeyUsage
--
--         , requestStopKeyUsage $
--             newStopKeyUsage
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--           ]

--     , testGroup "response"
--         [ responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateKey $
--             newCreateKeyResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDeleteKey $
--             newDeleteKeyResponse
--
--         , responseExportKey $
--             newExportKeyResponse
--
--         , responseGetAlias $
--             newGetAliasResponse
--
--         , responseGetKey $
--             newGetKeyResponse
--
--         , responseGetParametersForExport $
--             newGetParametersForExportResponse
--
--         , responseGetParametersForImport $
--             newGetParametersForImportResponse
--
--         , responseGetPublicKeyCertificate $
--             newGetPublicKeyCertificateResponse
--
--         , responseImportKey $
--             newImportKeyResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseListKeys $
--             newListKeysResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRestoreKey $
--             newRestoreKeyResponse
--
--         , responseStartKeyUsage $
--             newStartKeyUsageResponse
--
--         , responseStopKeyUsage $
--             newStopKeyUsageResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--           ]
--     ]

-- Requests

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateKey :: CreateKey -> TestTree
requestCreateKey =
  req
    "CreateKey"
    "fixture/CreateKey.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDeleteKey :: DeleteKey -> TestTree
requestDeleteKey =
  req
    "DeleteKey"
    "fixture/DeleteKey.yaml"

requestExportKey :: ExportKey -> TestTree
requestExportKey =
  req
    "ExportKey"
    "fixture/ExportKey.yaml"

requestGetAlias :: GetAlias -> TestTree
requestGetAlias =
  req
    "GetAlias"
    "fixture/GetAlias.yaml"

requestGetKey :: GetKey -> TestTree
requestGetKey =
  req
    "GetKey"
    "fixture/GetKey.yaml"

requestGetParametersForExport :: GetParametersForExport -> TestTree
requestGetParametersForExport =
  req
    "GetParametersForExport"
    "fixture/GetParametersForExport.yaml"

requestGetParametersForImport :: GetParametersForImport -> TestTree
requestGetParametersForImport =
  req
    "GetParametersForImport"
    "fixture/GetParametersForImport.yaml"

requestGetPublicKeyCertificate :: GetPublicKeyCertificate -> TestTree
requestGetPublicKeyCertificate =
  req
    "GetPublicKeyCertificate"
    "fixture/GetPublicKeyCertificate.yaml"

requestImportKey :: ImportKey -> TestTree
requestImportKey =
  req
    "ImportKey"
    "fixture/ImportKey.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestListKeys :: ListKeys -> TestTree
requestListKeys =
  req
    "ListKeys"
    "fixture/ListKeys.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRestoreKey :: RestoreKey -> TestTree
requestRestoreKey =
  req
    "RestoreKey"
    "fixture/RestoreKey.yaml"

requestStartKeyUsage :: StartKeyUsage -> TestTree
requestStartKeyUsage =
  req
    "StartKeyUsage"
    "fixture/StartKeyUsage.yaml"

requestStopKeyUsage :: StopKeyUsage -> TestTree
requestStopKeyUsage =
  req
    "StopKeyUsage"
    "fixture/StopKeyUsage.yaml"

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

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

-- Responses

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateKey :: CreateKeyResponse -> TestTree
responseCreateKey =
  res
    "CreateKeyResponse"
    "fixture/CreateKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKey)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseDeleteKey :: DeleteKeyResponse -> TestTree
responseDeleteKey =
  res
    "DeleteKeyResponse"
    "fixture/DeleteKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKey)

responseExportKey :: ExportKeyResponse -> TestTree
responseExportKey =
  res
    "ExportKeyResponse"
    "fixture/ExportKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportKey)

responseGetAlias :: GetAliasResponse -> TestTree
responseGetAlias =
  res
    "GetAliasResponse"
    "fixture/GetAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlias)

responseGetKey :: GetKeyResponse -> TestTree
responseGetKey =
  res
    "GetKeyResponse"
    "fixture/GetKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKey)

responseGetParametersForExport :: GetParametersForExportResponse -> TestTree
responseGetParametersForExport =
  res
    "GetParametersForExportResponse"
    "fixture/GetParametersForExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersForExport)

responseGetParametersForImport :: GetParametersForImportResponse -> TestTree
responseGetParametersForImport =
  res
    "GetParametersForImportResponse"
    "fixture/GetParametersForImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersForImport)

responseGetPublicKeyCertificate :: GetPublicKeyCertificateResponse -> TestTree
responseGetPublicKeyCertificate =
  res
    "GetPublicKeyCertificateResponse"
    "fixture/GetPublicKeyCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKeyCertificate)

responseImportKey :: ImportKeyResponse -> TestTree
responseImportKey =
  res
    "ImportKeyResponse"
    "fixture/ImportKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKey)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseListKeys :: ListKeysResponse -> TestTree
responseListKeys =
  res
    "ListKeysResponse"
    "fixture/ListKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeys)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRestoreKey :: RestoreKeyResponse -> TestTree
responseRestoreKey =
  res
    "RestoreKeyResponse"
    "fixture/RestoreKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreKey)

responseStartKeyUsage :: StartKeyUsageResponse -> TestTree
responseStartKeyUsage =
  res
    "StartKeyUsageResponse"
    "fixture/StartKeyUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartKeyUsage)

responseStopKeyUsage :: StopKeyUsageResponse -> TestTree
responseStopKeyUsage =
  res
    "StopKeyUsageResponse"
    "fixture/StopKeyUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopKeyUsage)

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

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)
