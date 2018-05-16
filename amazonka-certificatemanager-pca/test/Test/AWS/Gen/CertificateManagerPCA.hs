{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManagerPCA
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CertificateManagerPCA where

import Data.Proxy
import Network.AWS.CertificateManagerPCA
import Test.AWS.CertificateManagerPCA.Internal
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
--         [ requestImportCertificateAuthorityCertificate $
--             importCertificateAuthorityCertificate
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             describeCertificateAuthorityAuditReport
--
--         , requestRevokeCertificate $
--             revokeCertificate
--
--         , requestUpdateCertificateAuthority $
--             updateCertificateAuthority
--
--         , requestDeleteCertificateAuthority $
--             deleteCertificateAuthority
--
--         , requestGetCertificateAuthorityCSR $
--             getCertificateAuthorityCSR
--
--         , requestCreateCertificateAuthority $
--             createCertificateAuthority
--
--         , requestListCertificateAuthorities $
--             listCertificateAuthorities
--
--         , requestGetCertificate $
--             getCertificate
--
--         , requestTagCertificateAuthority $
--             tagCertificateAuthority
--
--         , requestDescribeCertificateAuthority $
--             describeCertificateAuthority
--
--         , requestIssueCertificate $
--             issueCertificate
--
--         , requestGetCertificateAuthorityCertificate $
--             getCertificateAuthorityCertificate
--
--         , requestUntagCertificateAuthority $
--             untagCertificateAuthority
--
--         , requestCreateCertificateAuthorityAuditReport $
--             createCertificateAuthorityAuditReport
--
--         , requestListTags $
--             listTags
--
--           ]

--     , testGroup "response"
--         [ responseImportCertificateAuthorityCertificate $
--             importCertificateAuthorityCertificateResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             describeCertificateAuthorityAuditReportResponse
--
--         , responseRevokeCertificate $
--             revokeCertificateResponse
--
--         , responseUpdateCertificateAuthority $
--             updateCertificateAuthorityResponse
--
--         , responseDeleteCertificateAuthority $
--             deleteCertificateAuthorityResponse
--
--         , responseGetCertificateAuthorityCSR $
--             getCertificateAuthorityCSRResponse
--
--         , responseCreateCertificateAuthority $
--             createCertificateAuthorityResponse
--
--         , responseListCertificateAuthorities $
--             listCertificateAuthoritiesResponse
--
--         , responseGetCertificate $
--             getCertificateResponse
--
--         , responseTagCertificateAuthority $
--             tagCertificateAuthorityResponse
--
--         , responseDescribeCertificateAuthority $
--             describeCertificateAuthorityResponse
--
--         , responseIssueCertificate $
--             issueCertificateResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             getCertificateAuthorityCertificateResponse
--
--         , responseUntagCertificateAuthority $
--             untagCertificateAuthorityResponse
--
--         , responseCreateCertificateAuthorityAuditReport $
--             createCertificateAuthorityAuditReportResponse
--
--         , responseListTags $
--             listTagsResponse
--
--           ]
--     ]

-- Requests

requestImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificate -> TestTree
requestImportCertificateAuthorityCertificate = req
    "ImportCertificateAuthorityCertificate"
    "fixture/ImportCertificateAuthorityCertificate.yaml"

requestDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReport -> TestTree
requestDescribeCertificateAuthorityAuditReport = req
    "DescribeCertificateAuthorityAuditReport"
    "fixture/DescribeCertificateAuthorityAuditReport.yaml"

requestRevokeCertificate :: RevokeCertificate -> TestTree
requestRevokeCertificate = req
    "RevokeCertificate"
    "fixture/RevokeCertificate.yaml"

requestUpdateCertificateAuthority :: UpdateCertificateAuthority -> TestTree
requestUpdateCertificateAuthority = req
    "UpdateCertificateAuthority"
    "fixture/UpdateCertificateAuthority.yaml"

requestDeleteCertificateAuthority :: DeleteCertificateAuthority -> TestTree
requestDeleteCertificateAuthority = req
    "DeleteCertificateAuthority"
    "fixture/DeleteCertificateAuthority.yaml"

requestGetCertificateAuthorityCSR :: GetCertificateAuthorityCSR -> TestTree
requestGetCertificateAuthorityCSR = req
    "GetCertificateAuthorityCSR"
    "fixture/GetCertificateAuthorityCSR.yaml"

requestCreateCertificateAuthority :: CreateCertificateAuthority -> TestTree
requestCreateCertificateAuthority = req
    "CreateCertificateAuthority"
    "fixture/CreateCertificateAuthority.yaml"

requestListCertificateAuthorities :: ListCertificateAuthorities -> TestTree
requestListCertificateAuthorities = req
    "ListCertificateAuthorities"
    "fixture/ListCertificateAuthorities.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate = req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestTagCertificateAuthority :: TagCertificateAuthority -> TestTree
requestTagCertificateAuthority = req
    "TagCertificateAuthority"
    "fixture/TagCertificateAuthority.yaml"

requestDescribeCertificateAuthority :: DescribeCertificateAuthority -> TestTree
requestDescribeCertificateAuthority = req
    "DescribeCertificateAuthority"
    "fixture/DescribeCertificateAuthority.yaml"

requestIssueCertificate :: IssueCertificate -> TestTree
requestIssueCertificate = req
    "IssueCertificate"
    "fixture/IssueCertificate.yaml"

requestGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificate -> TestTree
requestGetCertificateAuthorityCertificate = req
    "GetCertificateAuthorityCertificate"
    "fixture/GetCertificateAuthorityCertificate.yaml"

requestUntagCertificateAuthority :: UntagCertificateAuthority -> TestTree
requestUntagCertificateAuthority = req
    "UntagCertificateAuthority"
    "fixture/UntagCertificateAuthority.yaml"

requestCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReport -> TestTree
requestCreateCertificateAuthorityAuditReport = req
    "CreateCertificateAuthorityAuditReport"
    "fixture/CreateCertificateAuthorityAuditReport.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

-- Responses

responseImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificateResponse -> TestTree
responseImportCertificateAuthorityCertificate = res
    "ImportCertificateAuthorityCertificateResponse"
    "fixture/ImportCertificateAuthorityCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ImportCertificateAuthorityCertificate)

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport = res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DescribeCertificateAuthorityAuditReport)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate = res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy RevokeCertificate)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority = res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy UpdateCertificateAuthority)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority = res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DeleteCertificateAuthority)

responseGetCertificateAuthorityCSR :: GetCertificateAuthorityCSRResponse -> TestTree
responseGetCertificateAuthorityCSR = res
    "GetCertificateAuthorityCSRResponse"
    "fixture/GetCertificateAuthorityCSRResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificateAuthorityCSR)

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority = res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy CreateCertificateAuthority)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities = res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ListCertificateAuthorities)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate = res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificate)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority = res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy TagCertificateAuthority)

responseDescribeCertificateAuthority :: DescribeCertificateAuthorityResponse -> TestTree
responseDescribeCertificateAuthority = res
    "DescribeCertificateAuthorityResponse"
    "fixture/DescribeCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DescribeCertificateAuthority)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate = res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy IssueCertificate)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate = res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificateAuthorityCertificate)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority = res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy UntagCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport = res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy CreateCertificateAuthorityAuditReport)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ListTags)
