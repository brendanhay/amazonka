{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CertificateManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestResendValidationEmail $
--             newResendValidationEmail
--
--         , requestUpdateCertificateOptions $
--             newUpdateCertificateOptions
--
--         , requestListTagsForCertificate $
--             newListTagsForCertificate
--
--         , requestGetCertificate $
--             newGetCertificate
--
--         , requestAddTagsToCertificate $
--             newAddTagsToCertificate
--
--         , requestRequestCertificate $
--             newRequestCertificate
--
--         , requestListCertificates $
--             newListCertificates
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestRemoveTagsFromCertificate $
--             newRemoveTagsFromCertificate
--
--         , requestGetAccountConfiguration $
--             newGetAccountConfiguration
--
--         , requestImportCertificate $
--             newImportCertificate
--
--         , requestPutAccountConfiguration $
--             newPutAccountConfiguration
--
--         , requestDescribeCertificate $
--             newDescribeCertificate
--
--         , requestRenewCertificate $
--             newRenewCertificate
--
--         , requestExportCertificate $
--             newExportCertificate
--
--           ]

--     , testGroup "response"
--         [ responseResendValidationEmail $
--             newResendValidationEmailResponse
--
--         , responseUpdateCertificateOptions $
--             newUpdateCertificateOptionsResponse
--
--         , responseListTagsForCertificate $
--             newListTagsForCertificateResponse
--
--         , responseGetCertificate $
--             newGetCertificateResponse
--
--         , responseAddTagsToCertificate $
--             newAddTagsToCertificateResponse
--
--         , responseRequestCertificate $
--             newRequestCertificateResponse
--
--         , responseListCertificates $
--             newListCertificatesResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseRemoveTagsFromCertificate $
--             newRemoveTagsFromCertificateResponse
--
--         , responseGetAccountConfiguration $
--             newGetAccountConfigurationResponse
--
--         , responseImportCertificate $
--             newImportCertificateResponse
--
--         , responsePutAccountConfiguration $
--             newPutAccountConfigurationResponse
--
--         , responseDescribeCertificate $
--             newDescribeCertificateResponse
--
--         , responseRenewCertificate $
--             newRenewCertificateResponse
--
--         , responseExportCertificate $
--             newExportCertificateResponse
--
--           ]
--     ]

-- Requests

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

requestListTagsForCertificate :: ListTagsForCertificate -> TestTree
requestListTagsForCertificate =
  req
    "ListTagsForCertificate"
    "fixture/ListTagsForCertificate.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate =
  req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestAddTagsToCertificate :: AddTagsToCertificate -> TestTree
requestAddTagsToCertificate =
  req
    "AddTagsToCertificate"
    "fixture/AddTagsToCertificate.yaml"

requestRequestCertificate :: RequestCertificate -> TestTree
requestRequestCertificate =
  req
    "RequestCertificate"
    "fixture/RequestCertificate.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates =
  req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestRemoveTagsFromCertificate :: RemoveTagsFromCertificate -> TestTree
requestRemoveTagsFromCertificate =
  req
    "RemoveTagsFromCertificate"
    "fixture/RemoveTagsFromCertificate.yaml"

requestGetAccountConfiguration :: GetAccountConfiguration -> TestTree
requestGetAccountConfiguration =
  req
    "GetAccountConfiguration"
    "fixture/GetAccountConfiguration.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestPutAccountConfiguration :: PutAccountConfiguration -> TestTree
requestPutAccountConfiguration =
  req
    "PutAccountConfiguration"
    "fixture/PutAccountConfiguration.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate =
  req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestRenewCertificate :: RenewCertificate -> TestTree
requestRenewCertificate =
  req
    "RenewCertificate"
    "fixture/RenewCertificate.yaml"

requestExportCertificate :: ExportCertificate -> TestTree
requestExportCertificate =
  req
    "ExportCertificate"
    "fixture/ExportCertificate.yaml"

-- Responses

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

responseListTagsForCertificate :: ListTagsForCertificateResponse -> TestTree
responseListTagsForCertificate =
  res
    "ListTagsForCertificateResponse"
    "fixture/ListTagsForCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForCertificate)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificate)

responseAddTagsToCertificate :: AddTagsToCertificateResponse -> TestTree
responseAddTagsToCertificate =
  res
    "AddTagsToCertificateResponse"
    "fixture/AddTagsToCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToCertificate)

responseRequestCertificate :: RequestCertificateResponse -> TestTree
responseRequestCertificate =
  res
    "RequestCertificateResponse"
    "fixture/RequestCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestCertificate)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificates)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseRemoveTagsFromCertificate :: RemoveTagsFromCertificateResponse -> TestTree
responseRemoveTagsFromCertificate =
  res
    "RemoveTagsFromCertificateResponse"
    "fixture/RemoveTagsFromCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromCertificate)

responseGetAccountConfiguration :: GetAccountConfigurationResponse -> TestTree
responseGetAccountConfiguration =
  res
    "GetAccountConfigurationResponse"
    "fixture/GetAccountConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountConfiguration)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificate)

responsePutAccountConfiguration :: PutAccountConfigurationResponse -> TestTree
responsePutAccountConfiguration =
  res
    "PutAccountConfigurationResponse"
    "fixture/PutAccountConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountConfiguration)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificate)

responseRenewCertificate :: RenewCertificateResponse -> TestTree
responseRenewCertificate =
  res
    "RenewCertificateResponse"
    "fixture/RenewCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewCertificate)

responseExportCertificate :: ExportCertificateResponse -> TestTree
responseExportCertificate =
  res
    "ExportCertificateResponse"
    "fixture/ExportCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportCertificate)
