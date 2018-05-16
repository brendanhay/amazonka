{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManager
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             resendValidationEmail
--
--         , requestUpdateCertificateOptions $
--             updateCertificateOptions
--
--         , requestListTagsForCertificate $
--             listTagsForCertificate
--
--         , requestGetCertificate $
--             getCertificate
--
--         , requestAddTagsToCertificate $
--             addTagsToCertificate
--
--         , requestRequestCertificate $
--             requestCertificate
--
--         , requestListCertificates $
--             listCertificates
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestRemoveTagsFromCertificate $
--             removeTagsFromCertificate
--
--         , requestImportCertificate $
--             importCertificate
--
--         , requestDescribeCertificate $
--             describeCertificate
--
--         , requestExportCertificate $
--             exportCertificate
--
--           ]

--     , testGroup "response"
--         [ responseResendValidationEmail $
--             resendValidationEmailResponse
--
--         , responseUpdateCertificateOptions $
--             updateCertificateOptionsResponse
--
--         , responseListTagsForCertificate $
--             listTagsForCertificateResponse
--
--         , responseGetCertificate $
--             getCertificateResponse
--
--         , responseAddTagsToCertificate $
--             addTagsToCertificateResponse
--
--         , responseRequestCertificate $
--             requestCertificateResponse
--
--         , responseListCertificates $
--             listCertificatesResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseRemoveTagsFromCertificate $
--             removeTagsFromCertificateResponse
--
--         , responseImportCertificate $
--             importCertificateResponse
--
--         , responseDescribeCertificate $
--             describeCertificateResponse
--
--         , responseExportCertificate $
--             exportCertificateResponse
--
--           ]
--     ]

-- Requests

requestResendValidationEmail :: ResendValidationEmail -> TestTree
requestResendValidationEmail = req
    "ResendValidationEmail"
    "fixture/ResendValidationEmail.yaml"

requestUpdateCertificateOptions :: UpdateCertificateOptions -> TestTree
requestUpdateCertificateOptions = req
    "UpdateCertificateOptions"
    "fixture/UpdateCertificateOptions.yaml"

requestListTagsForCertificate :: ListTagsForCertificate -> TestTree
requestListTagsForCertificate = req
    "ListTagsForCertificate"
    "fixture/ListTagsForCertificate.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate = req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestAddTagsToCertificate :: AddTagsToCertificate -> TestTree
requestAddTagsToCertificate = req
    "AddTagsToCertificate"
    "fixture/AddTagsToCertificate.yaml"

requestRequestCertificate :: RequestCertificate -> TestTree
requestRequestCertificate = req
    "RequestCertificate"
    "fixture/RequestCertificate.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestRemoveTagsFromCertificate :: RemoveTagsFromCertificate -> TestTree
requestRemoveTagsFromCertificate = req
    "RemoveTagsFromCertificate"
    "fixture/RemoveTagsFromCertificate.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate = req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestExportCertificate :: ExportCertificate -> TestTree
requestExportCertificate = req
    "ExportCertificate"
    "fixture/ExportCertificate.yaml"

-- Responses

responseResendValidationEmail :: ResendValidationEmailResponse -> TestTree
responseResendValidationEmail = res
    "ResendValidationEmailResponse"
    "fixture/ResendValidationEmailResponse.proto"
    certificateManager
    (Proxy :: Proxy ResendValidationEmail)

responseUpdateCertificateOptions :: UpdateCertificateOptionsResponse -> TestTree
responseUpdateCertificateOptions = res
    "UpdateCertificateOptionsResponse"
    "fixture/UpdateCertificateOptionsResponse.proto"
    certificateManager
    (Proxy :: Proxy UpdateCertificateOptions)

responseListTagsForCertificate :: ListTagsForCertificateResponse -> TestTree
responseListTagsForCertificate = res
    "ListTagsForCertificateResponse"
    "fixture/ListTagsForCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy ListTagsForCertificate)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate = res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy GetCertificate)

responseAddTagsToCertificate :: AddTagsToCertificateResponse -> TestTree
responseAddTagsToCertificate = res
    "AddTagsToCertificateResponse"
    "fixture/AddTagsToCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy AddTagsToCertificate)

responseRequestCertificate :: RequestCertificateResponse -> TestTree
responseRequestCertificate = res
    "RequestCertificateResponse"
    "fixture/RequestCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy RequestCertificate)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    certificateManager
    (Proxy :: Proxy ListCertificates)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy DeleteCertificate)

responseRemoveTagsFromCertificate :: RemoveTagsFromCertificateResponse -> TestTree
responseRemoveTagsFromCertificate = res
    "RemoveTagsFromCertificateResponse"
    "fixture/RemoveTagsFromCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy RemoveTagsFromCertificate)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate = res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy ImportCertificate)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy DescribeCertificate)

responseExportCertificate :: ExportCertificateResponse -> TestTree
responseExportCertificate = res
    "ExportCertificateResponse"
    "fixture/ExportCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy ExportCertificate)
