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

import Amazonka.CertificateManagerPCA
import qualified Data.Proxy as Proxy
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
--             newImportCertificateAuthorityCertificate
--
--         , requestCreatePermission $
--             newCreatePermission
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReport
--
--         , requestDeletePermission $
--             newDeletePermission
--
--         , requestRevokeCertificate $
--             newRevokeCertificate
--
--         , requestUpdateCertificateAuthority $
--             newUpdateCertificateAuthority
--
--         , requestDeleteCertificateAuthority $
--             newDeleteCertificateAuthority
--
--         , requestGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsr
--
--         , requestCreateCertificateAuthority $
--             newCreateCertificateAuthority
--
--         , requestListCertificateAuthorities $
--             newListCertificateAuthorities
--
--         , requestGetCertificate $
--             newGetCertificate
--
--         , requestTagCertificateAuthority $
--             newTagCertificateAuthority
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDescribeCertificateAuthority $
--             newDescribeCertificateAuthority
--
--         , requestRestoreCertificateAuthority $
--             newRestoreCertificateAuthority
--
--         , requestIssueCertificate $
--             newIssueCertificate
--
--         , requestGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificate
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestUntagCertificateAuthority $
--             newUntagCertificateAuthority
--
--         , requestCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReport
--
--         , requestListTags $
--             newListTags
--
--         , requestGetPolicy $
--             newGetPolicy
--
--           ]

--     , testGroup "response"
--         [ responseImportCertificateAuthorityCertificate $
--             newImportCertificateAuthorityCertificateResponse
--
--         , responseCreatePermission $
--             newCreatePermissionResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReportResponse
--
--         , responseDeletePermission $
--             newDeletePermissionResponse
--
--         , responseRevokeCertificate $
--             newRevokeCertificateResponse
--
--         , responseUpdateCertificateAuthority $
--             newUpdateCertificateAuthorityResponse
--
--         , responseDeleteCertificateAuthority $
--             newDeleteCertificateAuthorityResponse
--
--         , responseGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsrResponse
--
--         , responseCreateCertificateAuthority $
--             newCreateCertificateAuthorityResponse
--
--         , responseListCertificateAuthorities $
--             newListCertificateAuthoritiesResponse
--
--         , responseGetCertificate $
--             newGetCertificateResponse
--
--         , responseTagCertificateAuthority $
--             newTagCertificateAuthorityResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDescribeCertificateAuthority $
--             newDescribeCertificateAuthorityResponse
--
--         , responseRestoreCertificateAuthority $
--             newRestoreCertificateAuthorityResponse
--
--         , responseIssueCertificate $
--             newIssueCertificateResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificateResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseUntagCertificateAuthority $
--             newUntagCertificateAuthorityResponse
--
--         , responseCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReportResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
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

requestGetCertificateAuthorityCsr :: GetCertificateAuthorityCsr -> TestTree
requestGetCertificateAuthorityCsr =
  req
    "GetCertificateAuthorityCsr"
    "fixture/GetCertificateAuthorityCsr.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificateAuthorityCertificate)

responseCreatePermission :: CreatePermissionResponse -> TestTree
responseCreatePermission =
  res
    "CreatePermissionResponse"
    "fixture/CreatePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePermission)

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport =
  res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificateAuthorityAuditReport)

responseDeletePermission :: DeletePermissionResponse -> TestTree
responseDeletePermission =
  res
    "DeletePermissionResponse"
    "fixture/DeletePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermission)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate =
  res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeCertificate)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority =
  res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificateAuthority)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority =
  res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificateAuthority)

responseGetCertificateAuthorityCsr :: GetCertificateAuthorityCsrResponse -> TestTree
responseGetCertificateAuthorityCsr =
  res
    "GetCertificateAuthorityCsrResponse"
    "fixture/GetCertificateAuthorityCsrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificateAuthorityCsr)

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority =
  res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateAuthority)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities =
  res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificateAuthorities)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificate)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority =
  res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagCertificateAuthority)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDescribeCertificateAuthority :: DescribeCertificateAuthorityResponse -> TestTree
responseDescribeCertificateAuthority =
  res
    "DescribeCertificateAuthorityResponse"
    "fixture/DescribeCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificateAuthority)

responseRestoreCertificateAuthority :: RestoreCertificateAuthorityResponse -> TestTree
responseRestoreCertificateAuthority =
  res
    "RestoreCertificateAuthorityResponse"
    "fixture/RestoreCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreCertificateAuthority)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate =
  res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IssueCertificate)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate =
  res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificateAuthorityCertificate)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority =
  res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport =
  res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateAuthorityAuditReport)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)
