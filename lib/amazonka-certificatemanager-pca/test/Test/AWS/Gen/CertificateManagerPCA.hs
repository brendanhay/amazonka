{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManagerPCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestCreatePermission $
--             createPermission
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             describeCertificateAuthorityAuditReport
--
--         , requestDeletePermission $
--             deletePermission
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
--         , requestPutPolicy $
--             putPolicy
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestDescribeCertificateAuthority $
--             describeCertificateAuthority
--
--         , requestRestoreCertificateAuthority $
--             restoreCertificateAuthority
--
--         , requestIssueCertificate $
--             issueCertificate
--
--         , requestGetCertificateAuthorityCertificate $
--             getCertificateAuthorityCertificate
--
--         , requestListPermissions $
--             listPermissions
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
--         , requestGetPolicy $
--             getPolicy
--
--           ]

--     , testGroup "response"
--         [ responseImportCertificateAuthorityCertificate $
--             importCertificateAuthorityCertificateResponse
--
--         , responseCreatePermission $
--             createPermissionResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             describeCertificateAuthorityAuditReportResponse
--
--         , responseDeletePermission $
--             deletePermissionResponse
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
--         , responsePutPolicy $
--             putPolicyResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseDescribeCertificateAuthority $
--             describeCertificateAuthorityResponse
--
--         , responseRestoreCertificateAuthority $
--             restoreCertificateAuthorityResponse
--
--         , responseIssueCertificate $
--             issueCertificateResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             getCertificateAuthorityCertificateResponse
--
--         , responseListPermissions $
--             listPermissionsResponse
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
--         , responseGetPolicy $
--             getPolicyResponse
--
--           ]
--     ]

-- Requests

requestImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificate -> TestTree
requestImportCertificateAuthorityCertificate =
  req
    "ImportCertificateAuthorityCertificate"
    "fixture/ImportCertificateAuthorityCertificate.yaml"

requestCreatePermission :: CreatePermission -> TestTree
requestCreatePermission =
  req
    "CreatePermission"
    "fixture/CreatePermission.yaml"

requestDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReport -> TestTree
requestDescribeCertificateAuthorityAuditReport =
  req
    "DescribeCertificateAuthorityAuditReport"
    "fixture/DescribeCertificateAuthorityAuditReport.yaml"

requestDeletePermission :: DeletePermission -> TestTree
requestDeletePermission =
  req
    "DeletePermission"
    "fixture/DeletePermission.yaml"

requestRevokeCertificate :: RevokeCertificate -> TestTree
requestRevokeCertificate =
  req
    "RevokeCertificate"
    "fixture/RevokeCertificate.yaml"

requestUpdateCertificateAuthority :: UpdateCertificateAuthority -> TestTree
requestUpdateCertificateAuthority =
  req
    "UpdateCertificateAuthority"
    "fixture/UpdateCertificateAuthority.yaml"

requestDeleteCertificateAuthority :: DeleteCertificateAuthority -> TestTree
requestDeleteCertificateAuthority =
  req
    "DeleteCertificateAuthority"
    "fixture/DeleteCertificateAuthority.yaml"

requestGetCertificateAuthorityCSR :: GetCertificateAuthorityCSR -> TestTree
requestGetCertificateAuthorityCSR =
  req
    "GetCertificateAuthorityCSR"
    "fixture/GetCertificateAuthorityCSR.yaml"

requestCreateCertificateAuthority :: CreateCertificateAuthority -> TestTree
requestCreateCertificateAuthority =
  req
    "CreateCertificateAuthority"
    "fixture/CreateCertificateAuthority.yaml"

requestListCertificateAuthorities :: ListCertificateAuthorities -> TestTree
requestListCertificateAuthorities =
  req
    "ListCertificateAuthorities"
    "fixture/ListCertificateAuthorities.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate =
  req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestTagCertificateAuthority :: TagCertificateAuthority -> TestTree
requestTagCertificateAuthority =
  req
    "TagCertificateAuthority"
    "fixture/TagCertificateAuthority.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDescribeCertificateAuthority :: DescribeCertificateAuthority -> TestTree
requestDescribeCertificateAuthority =
  req
    "DescribeCertificateAuthority"
    "fixture/DescribeCertificateAuthority.yaml"

requestRestoreCertificateAuthority :: RestoreCertificateAuthority -> TestTree
requestRestoreCertificateAuthority =
  req
    "RestoreCertificateAuthority"
    "fixture/RestoreCertificateAuthority.yaml"

requestIssueCertificate :: IssueCertificate -> TestTree
requestIssueCertificate =
  req
    "IssueCertificate"
    "fixture/IssueCertificate.yaml"

requestGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificate -> TestTree
requestGetCertificateAuthorityCertificate =
  req
    "GetCertificateAuthorityCertificate"
    "fixture/GetCertificateAuthorityCertificate.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestUntagCertificateAuthority :: UntagCertificateAuthority -> TestTree
requestUntagCertificateAuthority =
  req
    "UntagCertificateAuthority"
    "fixture/UntagCertificateAuthority.yaml"

requestCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReport -> TestTree
requestCreateCertificateAuthorityAuditReport =
  req
    "CreateCertificateAuthorityAuditReport"
    "fixture/CreateCertificateAuthorityAuditReport.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

-- Responses

responseImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificateResponse -> TestTree
responseImportCertificateAuthorityCertificate =
  res
    "ImportCertificateAuthorityCertificateResponse"
    "fixture/ImportCertificateAuthorityCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ImportCertificateAuthorityCertificate)

responseCreatePermission :: CreatePermissionResponse -> TestTree
responseCreatePermission =
  res
    "CreatePermissionResponse"
    "fixture/CreatePermissionResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy CreatePermission)

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport =
  res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DescribeCertificateAuthorityAuditReport)

responseDeletePermission :: DeletePermissionResponse -> TestTree
responseDeletePermission =
  res
    "DeletePermissionResponse"
    "fixture/DeletePermissionResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DeletePermission)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate =
  res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy RevokeCertificate)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority =
  res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy UpdateCertificateAuthority)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority =
  res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DeleteCertificateAuthority)

responseGetCertificateAuthorityCSR :: GetCertificateAuthorityCSRResponse -> TestTree
responseGetCertificateAuthorityCSR =
  res
    "GetCertificateAuthorityCSRResponse"
    "fixture/GetCertificateAuthorityCSRResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificateAuthorityCSR)

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority =
  res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy CreateCertificateAuthority)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities =
  res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ListCertificateAuthorities)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificate)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority =
  res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy TagCertificateAuthority)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DeletePolicy)

responseDescribeCertificateAuthority :: DescribeCertificateAuthorityResponse -> TestTree
responseDescribeCertificateAuthority =
  res
    "DescribeCertificateAuthorityResponse"
    "fixture/DescribeCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy DescribeCertificateAuthority)

responseRestoreCertificateAuthority :: RestoreCertificateAuthorityResponse -> TestTree
responseRestoreCertificateAuthority =
  res
    "RestoreCertificateAuthorityResponse"
    "fixture/RestoreCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy RestoreCertificateAuthority)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate =
  res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy IssueCertificate)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate =
  res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetCertificateAuthorityCertificate)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ListPermissions)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority =
  res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy UntagCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport =
  res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy CreateCertificateAuthorityAuditReport)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy ListTags)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    certificateManagerPCA
    (Proxy :: Proxy GetPolicy)
