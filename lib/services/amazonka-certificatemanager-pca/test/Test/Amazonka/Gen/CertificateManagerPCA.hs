{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CertificateManagerPCA
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CertificateManagerPCA where

import Amazonka.CertificateManagerPCA
import qualified Data.Proxy as Proxy
import Test.Amazonka.CertificateManagerPCA.Internal
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
--         [ requestCreateCertificateAuthority $
--             newCreateCertificateAuthority
--
--         , requestCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReport
--
--         , requestCreatePermission $
--             newCreatePermission
--
--         , requestDeleteCertificateAuthority $
--             newDeleteCertificateAuthority
--
--         , requestDeletePermission $
--             newDeletePermission
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDescribeCertificateAuthority $
--             newDescribeCertificateAuthority
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReport
--
--         , requestGetCertificate $
--             newGetCertificate
--
--         , requestGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificate
--
--         , requestGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsr
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestImportCertificateAuthorityCertificate $
--             newImportCertificateAuthorityCertificate
--
--         , requestIssueCertificate $
--             newIssueCertificate
--
--         , requestListCertificateAuthorities $
--             newListCertificateAuthorities
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListTags $
--             newListTags
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestRestoreCertificateAuthority $
--             newRestoreCertificateAuthority
--
--         , requestRevokeCertificate $
--             newRevokeCertificate
--
--         , requestTagCertificateAuthority $
--             newTagCertificateAuthority
--
--         , requestUntagCertificateAuthority $
--             newUntagCertificateAuthority
--
--         , requestUpdateCertificateAuthority $
--             newUpdateCertificateAuthority
--
--           ]

--     , testGroup "response"
--         [ responseCreateCertificateAuthority $
--             newCreateCertificateAuthorityResponse
--
--         , responseCreateCertificateAuthorityAuditReport $
--             newCreateCertificateAuthorityAuditReportResponse
--
--         , responseCreatePermission $
--             newCreatePermissionResponse
--
--         , responseDeleteCertificateAuthority $
--             newDeleteCertificateAuthorityResponse
--
--         , responseDeletePermission $
--             newDeletePermissionResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDescribeCertificateAuthority $
--             newDescribeCertificateAuthorityResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             newDescribeCertificateAuthorityAuditReportResponse
--
--         , responseGetCertificate $
--             newGetCertificateResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             newGetCertificateAuthorityCertificateResponse
--
--         , responseGetCertificateAuthorityCsr $
--             newGetCertificateAuthorityCsrResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseImportCertificateAuthorityCertificate $
--             newImportCertificateAuthorityCertificateResponse
--
--         , responseIssueCertificate $
--             newIssueCertificateResponse
--
--         , responseListCertificateAuthorities $
--             newListCertificateAuthoritiesResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseRestoreCertificateAuthority $
--             newRestoreCertificateAuthorityResponse
--
--         , responseRevokeCertificate $
--             newRevokeCertificateResponse
--
--         , responseTagCertificateAuthority $
--             newTagCertificateAuthorityResponse
--
--         , responseUntagCertificateAuthority $
--             newUntagCertificateAuthorityResponse
--
--         , responseUpdateCertificateAuthority $
--             newUpdateCertificateAuthorityResponse
--
--           ]
--     ]

-- Requests

requestCreateCertificateAuthority :: CreateCertificateAuthority -> TestTree
requestCreateCertificateAuthority =
  req
    "CreateCertificateAuthority"
    "fixture/CreateCertificateAuthority.yaml"

requestCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReport -> TestTree
requestCreateCertificateAuthorityAuditReport =
  req
    "CreateCertificateAuthorityAuditReport"
    "fixture/CreateCertificateAuthorityAuditReport.yaml"

requestCreatePermission :: CreatePermission -> TestTree
requestCreatePermission =
  req
    "CreatePermission"
    "fixture/CreatePermission.yaml"

requestDeleteCertificateAuthority :: DeleteCertificateAuthority -> TestTree
requestDeleteCertificateAuthority =
  req
    "DeleteCertificateAuthority"
    "fixture/DeleteCertificateAuthority.yaml"

requestDeletePermission :: DeletePermission -> TestTree
requestDeletePermission =
  req
    "DeletePermission"
    "fixture/DeletePermission.yaml"

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

requestDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReport -> TestTree
requestDescribeCertificateAuthorityAuditReport =
  req
    "DescribeCertificateAuthorityAuditReport"
    "fixture/DescribeCertificateAuthorityAuditReport.yaml"

requestGetCertificate :: GetCertificate -> TestTree
requestGetCertificate =
  req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

requestGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificate -> TestTree
requestGetCertificateAuthorityCertificate =
  req
    "GetCertificateAuthorityCertificate"
    "fixture/GetCertificateAuthorityCertificate.yaml"

requestGetCertificateAuthorityCsr :: GetCertificateAuthorityCsr -> TestTree
requestGetCertificateAuthorityCsr =
  req
    "GetCertificateAuthorityCsr"
    "fixture/GetCertificateAuthorityCsr.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificate -> TestTree
requestImportCertificateAuthorityCertificate =
  req
    "ImportCertificateAuthorityCertificate"
    "fixture/ImportCertificateAuthorityCertificate.yaml"

requestIssueCertificate :: IssueCertificate -> TestTree
requestIssueCertificate =
  req
    "IssueCertificate"
    "fixture/IssueCertificate.yaml"

requestListCertificateAuthorities :: ListCertificateAuthorities -> TestTree
requestListCertificateAuthorities =
  req
    "ListCertificateAuthorities"
    "fixture/ListCertificateAuthorities.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestRestoreCertificateAuthority :: RestoreCertificateAuthority -> TestTree
requestRestoreCertificateAuthority =
  req
    "RestoreCertificateAuthority"
    "fixture/RestoreCertificateAuthority.yaml"

requestRevokeCertificate :: RevokeCertificate -> TestTree
requestRevokeCertificate =
  req
    "RevokeCertificate"
    "fixture/RevokeCertificate.yaml"

requestTagCertificateAuthority :: TagCertificateAuthority -> TestTree
requestTagCertificateAuthority =
  req
    "TagCertificateAuthority"
    "fixture/TagCertificateAuthority.yaml"

requestUntagCertificateAuthority :: UntagCertificateAuthority -> TestTree
requestUntagCertificateAuthority =
  req
    "UntagCertificateAuthority"
    "fixture/UntagCertificateAuthority.yaml"

requestUpdateCertificateAuthority :: UpdateCertificateAuthority -> TestTree
requestUpdateCertificateAuthority =
  req
    "UpdateCertificateAuthority"
    "fixture/UpdateCertificateAuthority.yaml"

-- Responses

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority =
  res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport =
  res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificateAuthorityAuditReport)

responseCreatePermission :: CreatePermissionResponse -> TestTree
responseCreatePermission =
  res
    "CreatePermissionResponse"
    "fixture/CreatePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePermission)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority =
  res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificateAuthority)

responseDeletePermission :: DeletePermissionResponse -> TestTree
responseDeletePermission =
  res
    "DeletePermissionResponse"
    "fixture/DeletePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermission)

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

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport =
  res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificateAuthorityAuditReport)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificate)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate =
  res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificateAuthorityCertificate)

responseGetCertificateAuthorityCsr :: GetCertificateAuthorityCsrResponse -> TestTree
responseGetCertificateAuthorityCsr =
  res
    "GetCertificateAuthorityCsrResponse"
    "fixture/GetCertificateAuthorityCsrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificateAuthorityCsr)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseImportCertificateAuthorityCertificate :: ImportCertificateAuthorityCertificateResponse -> TestTree
responseImportCertificateAuthorityCertificate =
  res
    "ImportCertificateAuthorityCertificateResponse"
    "fixture/ImportCertificateAuthorityCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCertificateAuthorityCertificate)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate =
  res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IssueCertificate)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities =
  res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCertificateAuthorities)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPolicy)

responseRestoreCertificateAuthority :: RestoreCertificateAuthorityResponse -> TestTree
responseRestoreCertificateAuthority =
  res
    "RestoreCertificateAuthorityResponse"
    "fixture/RestoreCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreCertificateAuthority)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate =
  res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeCertificate)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority =
  res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagCertificateAuthority)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority =
  res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagCertificateAuthority)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority =
  res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCertificateAuthority)
