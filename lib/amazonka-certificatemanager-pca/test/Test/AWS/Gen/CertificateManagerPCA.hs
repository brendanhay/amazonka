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
--             mkImportCertificateAuthorityCertificate
--
--         , requestCreatePermission $
--             mkCreatePermission
--
--         , requestDescribeCertificateAuthorityAuditReport $
--             mkDescribeCertificateAuthorityAuditReport
--
--         , requestDeletePermission $
--             mkDeletePermission
--
--         , requestRevokeCertificate $
--             mkRevokeCertificate
--
--         , requestUpdateCertificateAuthority $
--             mkUpdateCertificateAuthority
--
--         , requestDeleteCertificateAuthority $
--             mkDeleteCertificateAuthority
--
--         , requestGetCertificateAuthorityCSR $
--             mkGetCertificateAuthorityCSR
--
--         , requestCreateCertificateAuthority $
--             mkCreateCertificateAuthority
--
--         , requestListCertificateAuthorities $
--             mkListCertificateAuthorities
--
--         , requestGetCertificate $
--             mkGetCertificate
--
--         , requestTagCertificateAuthority $
--             mkTagCertificateAuthority
--
--         , requestPutPolicy $
--             mkPutPolicy
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestDescribeCertificateAuthority $
--             mkDescribeCertificateAuthority
--
--         , requestRestoreCertificateAuthority $
--             mkRestoreCertificateAuthority
--
--         , requestIssueCertificate $
--             mkIssueCertificate
--
--         , requestGetCertificateAuthorityCertificate $
--             mkGetCertificateAuthorityCertificate
--
--         , requestListPermissions $
--             mkListPermissions
--
--         , requestUntagCertificateAuthority $
--             mkUntagCertificateAuthority
--
--         , requestCreateCertificateAuthorityAuditReport $
--             mkCreateCertificateAuthorityAuditReport
--
--         , requestListTags $
--             mkListTags
--
--         , requestGetPolicy $
--             mkGetPolicy
--
--           ]

--     , testGroup "response"
--         [ responseImportCertificateAuthorityCertificate $
--             mkImportCertificateAuthorityCertificateResponse
--
--         , responseCreatePermission $
--             mkCreatePermissionResponse
--
--         , responseDescribeCertificateAuthorityAuditReport $
--             mkDescribeCertificateAuthorityAuditReportResponse
--
--         , responseDeletePermission $
--             mkDeletePermissionResponse
--
--         , responseRevokeCertificate $
--             mkRevokeCertificateResponse
--
--         , responseUpdateCertificateAuthority $
--             mkUpdateCertificateAuthorityResponse
--
--         , responseDeleteCertificateAuthority $
--             mkDeleteCertificateAuthorityResponse
--
--         , responseGetCertificateAuthorityCSR $
--             mkGetCertificateAuthorityCSRResponse
--
--         , responseCreateCertificateAuthority $
--             mkCreateCertificateAuthorityResponse
--
--         , responseListCertificateAuthorities $
--             mkListCertificateAuthoritiesResponse
--
--         , responseGetCertificate $
--             mkGetCertificateResponse
--
--         , responseTagCertificateAuthority $
--             mkTagCertificateAuthorityResponse
--
--         , responsePutPolicy $
--             mkPutPolicyResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseDescribeCertificateAuthority $
--             mkDescribeCertificateAuthorityResponse
--
--         , responseRestoreCertificateAuthority $
--             mkRestoreCertificateAuthorityResponse
--
--         , responseIssueCertificate $
--             mkIssueCertificateResponse
--
--         , responseGetCertificateAuthorityCertificate $
--             mkGetCertificateAuthorityCertificateResponse
--
--         , responseListPermissions $
--             mkListPermissionsResponse
--
--         , responseUntagCertificateAuthority $
--             mkUntagCertificateAuthorityResponse
--
--         , responseCreateCertificateAuthorityAuditReport $
--             mkCreateCertificateAuthorityAuditReportResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseGetPolicy $
--             mkGetPolicyResponse
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
    certificateManagerPCAService
    (Proxy :: Proxy ImportCertificateAuthorityCertificate)

responseCreatePermission :: CreatePermissionResponse -> TestTree
responseCreatePermission =
  res
    "CreatePermissionResponse"
    "fixture/CreatePermissionResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy CreatePermission)

responseDescribeCertificateAuthorityAuditReport :: DescribeCertificateAuthorityAuditReportResponse -> TestTree
responseDescribeCertificateAuthorityAuditReport =
  res
    "DescribeCertificateAuthorityAuditReportResponse"
    "fixture/DescribeCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy DescribeCertificateAuthorityAuditReport)

responseDeletePermission :: DeletePermissionResponse -> TestTree
responseDeletePermission =
  res
    "DeletePermissionResponse"
    "fixture/DeletePermissionResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy DeletePermission)

responseRevokeCertificate :: RevokeCertificateResponse -> TestTree
responseRevokeCertificate =
  res
    "RevokeCertificateResponse"
    "fixture/RevokeCertificateResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy RevokeCertificate)

responseUpdateCertificateAuthority :: UpdateCertificateAuthorityResponse -> TestTree
responseUpdateCertificateAuthority =
  res
    "UpdateCertificateAuthorityResponse"
    "fixture/UpdateCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy UpdateCertificateAuthority)

responseDeleteCertificateAuthority :: DeleteCertificateAuthorityResponse -> TestTree
responseDeleteCertificateAuthority =
  res
    "DeleteCertificateAuthorityResponse"
    "fixture/DeleteCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy DeleteCertificateAuthority)

responseGetCertificateAuthorityCSR :: GetCertificateAuthorityCSRResponse -> TestTree
responseGetCertificateAuthorityCSR =
  res
    "GetCertificateAuthorityCSRResponse"
    "fixture/GetCertificateAuthorityCSRResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy GetCertificateAuthorityCSR)

responseCreateCertificateAuthority :: CreateCertificateAuthorityResponse -> TestTree
responseCreateCertificateAuthority =
  res
    "CreateCertificateAuthorityResponse"
    "fixture/CreateCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy CreateCertificateAuthority)

responseListCertificateAuthorities :: ListCertificateAuthoritiesResponse -> TestTree
responseListCertificateAuthorities =
  res
    "ListCertificateAuthoritiesResponse"
    "fixture/ListCertificateAuthoritiesResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy ListCertificateAuthorities)

responseGetCertificate :: GetCertificateResponse -> TestTree
responseGetCertificate =
  res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy GetCertificate)

responseTagCertificateAuthority :: TagCertificateAuthorityResponse -> TestTree
responseTagCertificateAuthority =
  res
    "TagCertificateAuthorityResponse"
    "fixture/TagCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy TagCertificateAuthority)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy DeletePolicy)

responseDescribeCertificateAuthority :: DescribeCertificateAuthorityResponse -> TestTree
responseDescribeCertificateAuthority =
  res
    "DescribeCertificateAuthorityResponse"
    "fixture/DescribeCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy DescribeCertificateAuthority)

responseRestoreCertificateAuthority :: RestoreCertificateAuthorityResponse -> TestTree
responseRestoreCertificateAuthority =
  res
    "RestoreCertificateAuthorityResponse"
    "fixture/RestoreCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy RestoreCertificateAuthority)

responseIssueCertificate :: IssueCertificateResponse -> TestTree
responseIssueCertificate =
  res
    "IssueCertificateResponse"
    "fixture/IssueCertificateResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy IssueCertificate)

responseGetCertificateAuthorityCertificate :: GetCertificateAuthorityCertificateResponse -> TestTree
responseGetCertificateAuthorityCertificate =
  res
    "GetCertificateAuthorityCertificateResponse"
    "fixture/GetCertificateAuthorityCertificateResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy GetCertificateAuthorityCertificate)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy ListPermissions)

responseUntagCertificateAuthority :: UntagCertificateAuthorityResponse -> TestTree
responseUntagCertificateAuthority =
  res
    "UntagCertificateAuthorityResponse"
    "fixture/UntagCertificateAuthorityResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy UntagCertificateAuthority)

responseCreateCertificateAuthorityAuditReport :: CreateCertificateAuthorityAuditReportResponse -> TestTree
responseCreateCertificateAuthorityAuditReport =
  res
    "CreateCertificateAuthorityAuditReportResponse"
    "fixture/CreateCertificateAuthorityAuditReportResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy CreateCertificateAuthorityAuditReport)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy ListTags)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    certificateManagerPCAService
    (Proxy :: Proxy GetPolicy)
