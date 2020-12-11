{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CertificateManager where

import Data.Proxy
import Network.AWS.CertificateManager
import Test.AWS.CertificateManager.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestResendValidationEmail $
--             mkResendValidationEmail
--
--         , requestUpdateCertificateOptions $
--             mkUpdateCertificateOptions
--
--         , requestListTagsForCertificate $
--             mkListTagsForCertificate
--
--         , requestGetCertificate $
--             mkGetCertificate
--
--         , requestAddTagsToCertificate $
--             mkAddTagsToCertificate
--
--         , requestRequestCertificate $
--             mkRequestCertificate
--
--         , requestListCertificates $
--             mkListCertificates
--
--         , requestDeleteCertificate $
--             mkDeleteCertificate
--
--         , requestRemoveTagsFromCertificate $
--             mkRemoveTagsFromCertificate
--
--         , requestImportCertificate $
--             mkImportCertificate
--
--         , requestDescribeCertificate $
--             mkDescribeCertificate
--
--         , requestRenewCertificate $
--             mkRenewCertificate
--
--         , requestExportCertificate $
--             mkExportCertificate
--
--           ]

--     , testGroup "response"
--         [ responseResendValidationEmail $
--             mkResendValidationEmailResponse
--
--         , responseUpdateCertificateOptions $
--             mkUpdateCertificateOptionsResponse
--
--         , responseListTagsForCertificate $
--             mkListTagsForCertificateResponse
--
--         , responseGetCertificate $
--             mkGetCertificateResponse
--
--         , responseAddTagsToCertificate $
--             mkAddTagsToCertificateResponse
--
--         , responseRequestCertificate $
--             mkRequestCertificateResponse
--
--         , responseListCertificates $
--             mkListCertificatesResponse
--
--         , responseDeleteCertificate $
--             mkDeleteCertificateResponse
--
--         , responseRemoveTagsFromCertificate $
--             mkRemoveTagsFromCertificateResponse
--
--         , responseImportCertificate $
--             mkImportCertificateResponse
--
--         , responseDescribeCertificate $
--             mkDescribeCertificateResponse
--
--         , responseRenewCertificate $
--             mkRenewCertificateResponse
--
--         , responseExportCertificate $
--             mkExportCertificateResponse
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

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate =
  req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

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
    certificateManagerService
    (Proxy :: Proxy ResendValidationEmail)

responseUpdateCertificateOptions :: UpdateCertificateOptionsResponse -> TestTree
responseUpdateCertificateOptions =
  res
    "UpdateCertificateOptionsResponse"
    "fixture/UpdateCertificateOptionsResponse.proto"
    certificateManagerService
    (Proxy :: Proxy UpdateCertificateOptions)

responseListTagsForCertificate :: ListTagsForCertificateResponse -> TestTree
responseListTagsForCertificate =
  res
    "ListTagsForCertificateResponse"
    "fixture/ListTagsForCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy ListTagsForCertificate)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy GetCertificate)

responseAddTagsToCertificate :: AddTagsToCertificateResponse -> TestTree
responseAddTagsToCertificate =
  res
    "AddTagsToCertificateResponse"
    "fixture/AddTagsToCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy AddTagsToCertificate)

responseRequestCertificate :: RequestCertificateResponse -> TestTree
responseRequestCertificate =
  res
    "RequestCertificateResponse"
    "fixture/RequestCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy RequestCertificate)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates =
  res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    certificateManagerService
    (Proxy :: Proxy ListCertificates)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy DeleteCertificate)

responseRemoveTagsFromCertificate :: RemoveTagsFromCertificateResponse -> TestTree
responseRemoveTagsFromCertificate =
  res
    "RemoveTagsFromCertificateResponse"
    "fixture/RemoveTagsFromCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy RemoveTagsFromCertificate)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate =
  res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy ImportCertificate)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate =
  res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy DescribeCertificate)

responseRenewCertificate :: RenewCertificateResponse -> TestTree
responseRenewCertificate =
  res
    "RenewCertificateResponse"
    "fixture/RenewCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy RenewCertificate)

responseExportCertificate :: ExportCertificateResponse -> TestTree
responseExportCertificate =
  res
    "ExportCertificateResponse"
    "fixture/ExportCertificateResponse.proto"
    certificateManagerService
    (Proxy :: Proxy ExportCertificate)
