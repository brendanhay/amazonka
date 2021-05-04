{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManagerPCA
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreatePermission $
--             newCreatePermission
--
--         , requestRestoreCertificateAuthority $
--             newRestoreCertificateAuthority
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDescribeCertificateAuthority $
--             newDescribeCertificateAuthority
--
--         , requestTagCertificateAuthority $
--             newTagCertificateAuthority
--
--         , requestCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReport
--
--         , requestGetCertificate $
--             newGetCertificate
--
--         , requestCreateCertificateAuthority $
--             newCreateCertificateAuthority
--
--         , requestGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsr
--
--         , requestListCertificateAuthorities $
--             newListCertificateAuthorities
--
--         , requestRevokeCertificate $
--             newRevokeCertificate
--
--         , requestDeletePermission $
--             newDeletePermission
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificate
--
--         , requestIssueCertificate $
--             newIssueCertificate
--
--         , requestImportCertificateAuthorityCertificate $
--             newImportCertificateAuthorityCertificate
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteCertificateAuthority $
--             newDeleteCertificateAuthority
--
--         , requestUpdateCertificateAuthority $
--             newUpdateCertificateAuthority
--
--         , requestUntagCertificateAuthority $
--             newUntagCertificateAuthority
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReport
--
--           ]

--     , testGroup "response"
--         [ responseCreatePermission $
--             newCreatePermissionResponse
--
--         , responseRestoreCertificateAuthority $
--             newRestoreCertificateAuthorityResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDescribeCertificateAuthority $
--             newDescribeCertificateAuthorityResponse
--
--         , responseTagCertificateAuthority $
--             newTagCertificateAuthorityResponse
--
--         , responseCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReportResponse
--
--         , responseGetCertificate $
--             newGetCertificateResponse
--
--         , responseCreateCertificateAuthority $
--             newCreateCertificateAuthorityResponse
--
--         , responseGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsrResponse
--
--         , responseListCertificateAuthorities $
--             newListCertificateAuthoritiesResponse
--
--         , responseRevokeCertificate $
--             newRevokeCertificateResponse
--
--         , responseDeletePermission $
--             newDeletePermissionResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificateResponse
--
--         , responseIssueCertificate $
--             newIssueCertificateResponse
--
--         , responseImportCertificateAuthorityCertificate $
--             newImportCertificateAuthorityCertificateResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteCertificateAuthority $
--             newDeleteCertificateAuthorityResponse
--
--         , responseUpdateCertificateAuthority $
--             newUpdateCertificateAuthorityResponse
--
--         , responseUntagCertificateAuthority $
--             newUntagCertificateAuthorityResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReportResponse
--
--           ]
--     ]

-- Requests

requestCreatePermission :: CreatePermission -> TestTree
requestCreatePermission =
  req
    "CreatePermission"
    "fixture/CreatePermission.yaml"

requestRestoreCertificateAuthority :: RestoreCertificateAuthority -> TestTree
requestRestoreCertificateAuthority =
  req
    "RestoreCertificateAuthority"
    "fixture/RestoreCertificateAuthority.yaml"

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

requestTagCertificateAuthority :: TagCertificateAuthority -> TestTree
requestTagCertificateAuthority =
  req
    "TagCertificateAuthority"
    "fixture/TagCertificateAuthority.yaml"

requestCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReport -> TestTree
requestCreateCertificateAuthorityAuditReport =
  req
    "CreateCertificateAuthorityAuditReport"
    "fixture/CreateCertificateAuthorityAuditReport.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate =
  req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestCreateCertificateAuthority :: CreateCertificateAuthority -> TestTree
requestCreateCertificateAuthority =
  req
    "CreateCertificateAuthority"
    "fixture/CreateCertificateAuthority.yaml"

requestGetCertificateAuthorityCsr :: GetCertificateAuthorityCsr -> TestTree
requestGetCertificateAuthorityCsr =
  req
    "GetCertificateAuthorityCsr"
    "fixture/GetCertificateAuthorityCsr.yaml"

requestListCertificateAuthorities :: ListCertificateAuthorities -> TestTree
requestListCertificateAuthorities =
  req
    "ListCertificateAuthorities"
    "fixture/ListCertificateAuthorities.yaml"

requestRevokeCertificate :: RevokeCertificate -> TestTree
requestRevokeCertificate =
  req
    "RevokeCertificate"
    "fixture/RevokeCertificate.yaml"

requestDeletePermission :: DeletePermission -> TestTree
requestDeletePermission =
  req
    "DeletePermission"
    "fixture/DeletePermission.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificate -> TestTree
requestGetCertificateAuthorityCertificate =
  req
    "GetCertificateAuthorityCertificate"
    "fixture/GetCertificateAuthorityCertificate.yaml"

requestIssueCertificate :: IssueCertificate -> TestTree
requestIssueCertificate =
  req
    "IssueCertificate"
    "fixture/IssueCertificate.yaml"

requestImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificate -> TestTree
requestImportCertificateAuthorityCertificate =
  req
    "ImportCertificateAuthorityCertificate"
    "fixture/ImportCertificateAuthorityCertificate.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteCertificateAuthority :: DeleteCertificateAuthority -> TestTree
requestDeleteCertificateAuthority =
  req
    "DeleteCertificateAuthority"
    "fixture/DeleteCertificateAuthority.yaml"

requestUpdateCertificateAuthority :: UpdateCertificateAuthority -> TestTree
requestUpdateCertificateAuthority =
  req
    "UpdateCertificateAuthority"
    "fixture/UpdateCertificateAuthority.yaml"

requestUntagCertificateAuthority :: UntagCertificateAuthority -> TestTree
requestUntagCertificateAuthority =
  req
    "UntagCertificateAuthority"
    "fixture/UntagCertificateAuthority.yaml"

requestDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReport -> TestTree
requestDescribeCertificateAuthorityAuditReport =
  req
    "DescribeCertificateAuthorityAuditReport"
    "fixture/DescribeCertificateAuthorityAuditReport.yaml"

-- Responses

responseCreatePermission :: CreatePermissionResponse -> TestTree
responseCreatePermission =
  res
    "CreatePermissionResponse"
    "fixture/CreatePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePermission)

responseRestoreCertificateAuthority :: RestoreCertificateAuthorityResponse -> TestTree
responseRestoreCertificateAuthority =
  res
    "RestoreCertificateAuthorityResponse"
    "fixture/RestoreCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreCertificateAuthority)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseDescribeCertificateAuthority :: DescribeCertificateAuthorityResponse -> TestTree
responseDescribeCertificateAuthority =
  res
    "DescribeCertificateAuthorityResponse"
    "fixture/DescribeCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificateAuthority)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority =
  res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy TagCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport =
  res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificateAuthorityAuditReport)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCertificate)

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority =
  res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificateAuthority)

responseGetCertificateAuthorityCsr :: GetCertificateAuthorityCsrResponse -> TestTree
responseGetCertificateAuthorityCsr =
  res
    "GetCertificateAuthorityCsrResponse"
    "fixture/GetCertificateAuthorityCsrResponse.proto"
    defaultService
    (Proxy :: Proxy GetCertificateAuthorityCsr)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities =
  res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCertificateAuthorities)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate =
  res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeCertificate)

responseDeletePermission :: DeletePermissionResponse -> TestTree
responseDeletePermission =
  res
    "DeletePermissionResponse"
    "fixture/DeletePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePermission)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPermissions)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate =
  res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCertificateAuthorityCertificate)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate =
  res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy IssueCertificate)

responseImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificateResponse -> TestTree
responseImportCertificateAuthorityCertificate =
  res
    "ImportCertificateAuthorityCertificateResponse"
    "fixture/ImportCertificateAuthorityCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy ImportCertificateAuthorityCertificate)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutPolicy)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPolicy)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority =
  res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificateAuthority)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority =
  res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCertificateAuthority)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority =
  res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy UntagCertificateAuthority)

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport =
  res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificateAuthorityAuditReport)
