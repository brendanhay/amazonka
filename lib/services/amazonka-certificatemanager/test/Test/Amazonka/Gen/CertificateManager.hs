{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CertificateManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CertificateManager where

import Amazonka.CertificateManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.CertificateManager.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTagsToCertificate $
--             newAddTagsToCertificate
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestExportCertificate $
--             newExportCertificate
--
--         , requestGetAccountConfiguration $
--             newGetAccountConfiguration
--
--         , requestGetCertificate $
--             newGetCertificate
--
--         , requestImportCertificate $
--             newImportCertificate
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestListTagsForCertificate $
--             newListTagsForCertificate
--
--         , requestPutAccountConfiguration $
--             newPutAccountConfiguration
--
--         , requestRemoveTagsFromCertificate $
--             newRemoveTagsFromCertificate
--
--         , requestRenewCertificate $
--             newRenewCertificate
--
--         , requestRequestCertificate $
--             newRequestCertificate
--
--         , requestResendValidationEmail $
--             newResendValidationEmail
--
--         , requestUpdateCertificateOptions $
--             newUpdateCertificateOptions
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToCertificate $
--             newAddTagsToCertificateResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseExportCertificate $
--             newExportCertificateResponse
--
--         , responseGetAccountConfiguration $
--             newGetAccountConfigurationResponse
--
--         , responseGetCertificate $
--             newGetCertificateResponse
--
--         , responseImportCertificate $
--             newImportCertificateResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseListTagsForCertificate $
--             newListTagsForCertificateResponse
--
--         , responsePutAccountConfiguration $
--             newPutAccountConfigurationResponse
--
--         , responseRemoveTagsFromCertificate $
--             newRemoveTagsFromCertificateResponse
--
--         , responseRenewCertificate $
--             newRenewCertificateResponse
--
--         , responseRequestCertificate $
--             newRequestCertificateResponse
--
--         , responseResendValidationEmail $
--             newResendValidationEmailResponse
--
--         , responseUpdateCertificateOptions $
--             newUpdateCertificateOptionsResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToCertificate :: AddTagsToCertificate -> TestTree
requestAddTagsToCertificate =
  req
    "AddTagsToCertificate"
    "fixture/AddTagsToCertificate.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestExportCertificate :: ExportCertificate -> TestTree
requestExportCertificate =
  req
    "ExportCertificate"
    "fixture/ExportCertificate.yaml"

requestGetAccountConfiguration :: GetAccountConfiguration -> TestTree
requestGetAccountConfiguration =
  req
    "GetAccountConfiguration"
    "fixture/GetAccountConfiguration.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate =
  req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestListTagsForCertificate :: ListTagsForCertificate -> TestTree
requestListTagsForCertificate =
  req
    "ListTagsForCertificate"
    "fixture/ListTagsForCertificate.yaml"

requestPutAccountConfiguration :: PutAccountConfiguration -> TestTree
requestPutAccountConfiguration =
  req
    "PutAccountConfiguration"
    "fixture/PutAccountConfiguration.yaml"

requestRemoveTagsFromCertificate :: RemoveTagsFromCertificate -> TestTree
requestRemoveTagsFromCertificate =
  req
    "RemoveTagsFromCertificate"
    "fixture/RemoveTagsFromCertificate.yaml"

requestRenewCertificate :: RenewCertificate -> TestTree
requestRenewCertificate =
  req
    "RenewCertificate"
    "fixture/RenewCertificate.yaml"

requestRequestCertificate :: RequestCertificate -> TestTree
requestRequestCertificate =
  req
    "RequestCertificate"
    "fixture/RequestCertificate.yaml"

requestResendValidationEmail :: ResendValidationEmail -> TestTree
requestResendValidationEmail =
  req
    "ResendValidationEmail"
    "fixture/ResendValidationEmail.yaml"

requestUpdateCertificateOptions :: UpdateCertificateOptions -> TestTree
requestUpdateCertificateOptions =
  req
    "UpdateCertificateOptions"
    "fixture/UpdateCertificateOptions.yaml"

-- Responses

responseAddTagsToCertificate :: AddTagsToCertificateResponse -> TestTree
responseAddTagsToCertificate =
  res
    "AddTagsToCertificateResponse"
    "fixture/AddTagsToCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToCertificate)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseExportCertificate :: ExportCertificateResponse -> TestTree
responseExportCertificate =
  res
    "ExportCertificateResponse"
    "fixture/ExportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportCertificate)

responseGetAccountConfiguration :: GetAccountConfigurationResponse -> TestTree
responseGetAccountConfiguration =
  res
    "GetAccountConfigurationResponse"
    "fixture/GetAccountConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountConfiguration)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificate)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificate)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseListTagsForCertificate :: ListTagsForCertificateResponse -> TestTree
responseListTagsForCertificate =
  res
    "ListTagsForCertificateResponse"
    "fixture/ListTagsForCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForCertificate)

responsePutAccountConfiguration :: PutAccountConfigurationResponse -> TestTree
responsePutAccountConfiguration =
  res
    "PutAccountConfigurationResponse"
    "fixture/PutAccountConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountConfiguration)

responseRemoveTagsFromCertificate :: RemoveTagsFromCertificateResponse -> TestTree
responseRemoveTagsFromCertificate =
  res
    "RemoveTagsFromCertificateResponse"
    "fixture/RemoveTagsFromCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromCertificate)

responseRenewCertificate :: RenewCertificateResponse -> TestTree
responseRenewCertificate =
  res
    "RenewCertificateResponse"
    "fixture/RenewCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewCertificate)

responseRequestCertificate :: RequestCertificateResponse -> TestTree
responseRequestCertificate =
  res
    "RequestCertificateResponse"
    "fixture/RequestCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestCertificate)

responseResendValidationEmail :: ResendValidationEmailResponse -> TestTree
responseResendValidationEmail =
  res
    "ResendValidationEmailResponse"
    "fixture/ResendValidationEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendValidationEmail)

responseUpdateCertificateOptions :: UpdateCertificateOptionsResponse -> TestTree
responseUpdateCertificateOptions =
  res
    "UpdateCertificateOptionsResponse"
    "fixture/UpdateCertificateOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificateOptions)
