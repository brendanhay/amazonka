{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeArtifact where

import qualified Data.Proxy as Proxy
import Network.AWS.CodeArtifact
import Test.AWS.CodeArtifact.Internal
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
--         [ requestDisposePackageVersions $
--             newDisposePackageVersions
--
--         , requestGetRepositoryEndpoint $
--             newGetRepositoryEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListPackageVersionDependencies $
--             newListPackageVersionDependencies
--
--         , requestListPackages $
--             newListPackages
--
--         , requestPutRepositoryPermissionsPolicy $
--             newPutRepositoryPermissionsPolicy
--
--         , requestDeleteRepositoryPermissionsPolicy $
--             newDeleteRepositoryPermissionsPolicy
--
--         , requestGetDomainPermissionsPolicy $
--             newGetDomainPermissionsPolicy
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestUpdatePackageVersionsStatus $
--             newUpdatePackageVersionsStatus
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestGetPackageVersionAsset $
--             newGetPackageVersionAsset
--
--         , requestListRepositoriesInDomain $
--             newListRepositoriesInDomain
--
--         , requestPutDomainPermissionsPolicy $
--             newPutDomainPermissionsPolicy
--
--         , requestDeleteDomainPermissionsPolicy $
--             newDeleteDomainPermissionsPolicy
--
--         , requestGetPackageVersionReadme $
--             newGetPackageVersionReadme
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestGetRepositoryPermissionsPolicy $
--             newGetRepositoryPermissionsPolicy
--
--         , requestAssociateExternalConnection $
--             newAssociateExternalConnection
--
--         , requestDescribeRepository $
--             newDescribeRepository
--
--         , requestDeletePackageVersions $
--             newDeletePackageVersions
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribePackageVersion $
--             newDescribePackageVersion
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestUpdateRepository $
--             newUpdateRepository
--
--         , requestCopyPackageVersions $
--             newCopyPackageVersions
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestDisassociateExternalConnection $
--             newDisassociateExternalConnection
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListPackageVersions $
--             newListPackageVersions
--
--         , requestListPackageVersionAssets $
--             newListPackageVersionAssets
--
--           ]

--     , testGroup "response"
--         [ responseDisposePackageVersions $
--             newDisposePackageVersionsResponse
--
--         , responseGetRepositoryEndpoint $
--             newGetRepositoryEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListPackageVersionDependencies $
--             newListPackageVersionDependenciesResponse
--
--         , responseListPackages $
--             newListPackagesResponse
--
--         , responsePutRepositoryPermissionsPolicy $
--             newPutRepositoryPermissionsPolicyResponse
--
--         , responseDeleteRepositoryPermissionsPolicy $
--             newDeleteRepositoryPermissionsPolicyResponse
--
--         , responseGetDomainPermissionsPolicy $
--             newGetDomainPermissionsPolicyResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseUpdatePackageVersionsStatus $
--             newUpdatePackageVersionsStatusResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseGetPackageVersionAsset $
--             newGetPackageVersionAssetResponse
--
--         , responseListRepositoriesInDomain $
--             newListRepositoriesInDomainResponse
--
--         , responsePutDomainPermissionsPolicy $
--             newPutDomainPermissionsPolicyResponse
--
--         , responseDeleteDomainPermissionsPolicy $
--             newDeleteDomainPermissionsPolicyResponse
--
--         , responseGetPackageVersionReadme $
--             newGetPackageVersionReadmeResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseGetRepositoryPermissionsPolicy $
--             newGetRepositoryPermissionsPolicyResponse
--
--         , responseAssociateExternalConnection $
--             newAssociateExternalConnectionResponse
--
--         , responseDescribeRepository $
--             newDescribeRepositoryResponse
--
--         , responseDeletePackageVersions $
--             newDeletePackageVersionsResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribePackageVersion $
--             newDescribePackageVersionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseUpdateRepository $
--             newUpdateRepositoryResponse
--
--         , responseCopyPackageVersions $
--             newCopyPackageVersionsResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseDisassociateExternalConnection $
--             newDisassociateExternalConnectionResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListPackageVersions $
--             newListPackageVersionsResponse
--
--         , responseListPackageVersionAssets $
--             newListPackageVersionAssetsResponse
--
--           ]
--     ]

-- Requests

requestDisposePackageVersions :: DisposePackageVersions -> TestTree
requestDisposePackageVersions =
  req
    "DisposePackageVersions"
    "fixture/DisposePackageVersions.yaml"

requestGetRepositoryEndpoint :: GetRepositoryEndpoint -> TestTree
requestGetRepositoryEndpoint =
  req
    "GetRepositoryEndpoint"
    "fixture/GetRepositoryEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListPackageVersionDependencies :: ListPackageVersionDependencies -> TestTree
requestListPackageVersionDependencies =
  req
    "ListPackageVersionDependencies"
    "fixture/ListPackageVersionDependencies.yaml"

requestListPackages :: ListPackages -> TestTree
requestListPackages =
  req
    "ListPackages"
    "fixture/ListPackages.yaml"

requestPutRepositoryPermissionsPolicy :: PutRepositoryPermissionsPolicy -> TestTree
requestPutRepositoryPermissionsPolicy =
  req
    "PutRepositoryPermissionsPolicy"
    "fixture/PutRepositoryPermissionsPolicy.yaml"

requestDeleteRepositoryPermissionsPolicy :: DeleteRepositoryPermissionsPolicy -> TestTree
requestDeleteRepositoryPermissionsPolicy =
  req
    "DeleteRepositoryPermissionsPolicy"
    "fixture/DeleteRepositoryPermissionsPolicy.yaml"

requestGetDomainPermissionsPolicy :: GetDomainPermissionsPolicy -> TestTree
requestGetDomainPermissionsPolicy =
  req
    "GetDomainPermissionsPolicy"
    "fixture/GetDomainPermissionsPolicy.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestUpdatePackageVersionsStatus :: UpdatePackageVersionsStatus -> TestTree
requestUpdatePackageVersionsStatus =
  req
    "UpdatePackageVersionsStatus"
    "fixture/UpdatePackageVersionsStatus.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestGetPackageVersionAsset :: GetPackageVersionAsset -> TestTree
requestGetPackageVersionAsset =
  req
    "GetPackageVersionAsset"
    "fixture/GetPackageVersionAsset.yaml"

requestListRepositoriesInDomain :: ListRepositoriesInDomain -> TestTree
requestListRepositoriesInDomain =
  req
    "ListRepositoriesInDomain"
    "fixture/ListRepositoriesInDomain.yaml"

requestPutDomainPermissionsPolicy :: PutDomainPermissionsPolicy -> TestTree
requestPutDomainPermissionsPolicy =
  req
    "PutDomainPermissionsPolicy"
    "fixture/PutDomainPermissionsPolicy.yaml"

requestDeleteDomainPermissionsPolicy :: DeleteDomainPermissionsPolicy -> TestTree
requestDeleteDomainPermissionsPolicy =
  req
    "DeleteDomainPermissionsPolicy"
    "fixture/DeleteDomainPermissionsPolicy.yaml"

requestGetPackageVersionReadme :: GetPackageVersionReadme -> TestTree
requestGetPackageVersionReadme =
  req
    "GetPackageVersionReadme"
    "fixture/GetPackageVersionReadme.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestGetRepositoryPermissionsPolicy :: GetRepositoryPermissionsPolicy -> TestTree
requestGetRepositoryPermissionsPolicy =
  req
    "GetRepositoryPermissionsPolicy"
    "fixture/GetRepositoryPermissionsPolicy.yaml"

requestAssociateExternalConnection :: AssociateExternalConnection -> TestTree
requestAssociateExternalConnection =
  req
    "AssociateExternalConnection"
    "fixture/AssociateExternalConnection.yaml"

requestDescribeRepository :: DescribeRepository -> TestTree
requestDescribeRepository =
  req
    "DescribeRepository"
    "fixture/DescribeRepository.yaml"

requestDeletePackageVersions :: DeletePackageVersions -> TestTree
requestDeletePackageVersions =
  req
    "DeletePackageVersions"
    "fixture/DeletePackageVersions.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribePackageVersion :: DescribePackageVersion -> TestTree
requestDescribePackageVersion =
  req
    "DescribePackageVersion"
    "fixture/DescribePackageVersion.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestUpdateRepository :: UpdateRepository -> TestTree
requestUpdateRepository =
  req
    "UpdateRepository"
    "fixture/UpdateRepository.yaml"

requestCopyPackageVersions :: CopyPackageVersions -> TestTree
requestCopyPackageVersions =
  req
    "CopyPackageVersions"
    "fixture/CopyPackageVersions.yaml"

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken =
  req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

requestDisassociateExternalConnection :: DisassociateExternalConnection -> TestTree
requestDisassociateExternalConnection =
  req
    "DisassociateExternalConnection"
    "fixture/DisassociateExternalConnection.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListPackageVersions :: ListPackageVersions -> TestTree
requestListPackageVersions =
  req
    "ListPackageVersions"
    "fixture/ListPackageVersions.yaml"

requestListPackageVersionAssets :: ListPackageVersionAssets -> TestTree
requestListPackageVersionAssets =
  req
    "ListPackageVersionAssets"
    "fixture/ListPackageVersionAssets.yaml"

-- Responses

responseDisposePackageVersions :: DisposePackageVersionsResponse -> TestTree
responseDisposePackageVersions =
  res
    "DisposePackageVersionsResponse"
    "fixture/DisposePackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisposePackageVersions)

responseGetRepositoryEndpoint :: GetRepositoryEndpointResponse -> TestTree
responseGetRepositoryEndpoint =
  res
    "GetRepositoryEndpointResponse"
    "fixture/GetRepositoryEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListPackageVersionDependencies :: ListPackageVersionDependenciesResponse -> TestTree
responseListPackageVersionDependencies =
  res
    "ListPackageVersionDependenciesResponse"
    "fixture/ListPackageVersionDependenciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersionDependencies)

responseListPackages :: ListPackagesResponse -> TestTree
responseListPackages =
  res
    "ListPackagesResponse"
    "fixture/ListPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackages)

responsePutRepositoryPermissionsPolicy :: PutRepositoryPermissionsPolicyResponse -> TestTree
responsePutRepositoryPermissionsPolicy =
  res
    "PutRepositoryPermissionsPolicyResponse"
    "fixture/PutRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryPermissionsPolicy)

responseDeleteRepositoryPermissionsPolicy :: DeleteRepositoryPermissionsPolicyResponse -> TestTree
responseDeleteRepositoryPermissionsPolicy =
  res
    "DeleteRepositoryPermissionsPolicyResponse"
    "fixture/DeleteRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepositoryPermissionsPolicy)

responseGetDomainPermissionsPolicy :: GetDomainPermissionsPolicyResponse -> TestTree
responseGetDomainPermissionsPolicy =
  res
    "GetDomainPermissionsPolicyResponse"
    "fixture/GetDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainPermissionsPolicy)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositories)

responseUpdatePackageVersionsStatus :: UpdatePackageVersionsStatusResponse -> TestTree
responseUpdatePackageVersionsStatus =
  res
    "UpdatePackageVersionsStatusResponse"
    "fixture/UpdatePackageVersionsStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePackageVersionsStatus)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseListRepositoriesInDomain :: ListRepositoriesInDomainResponse -> TestTree
responseListRepositoriesInDomain =
  res
    "ListRepositoriesInDomainResponse"
    "fixture/ListRepositoriesInDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoriesInDomain)

responsePutDomainPermissionsPolicy :: PutDomainPermissionsPolicyResponse -> TestTree
responsePutDomainPermissionsPolicy =
  res
    "PutDomainPermissionsPolicyResponse"
    "fixture/PutDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDomainPermissionsPolicy)

responseDeleteDomainPermissionsPolicy :: DeleteDomainPermissionsPolicyResponse -> TestTree
responseDeleteDomainPermissionsPolicy =
  res
    "DeleteDomainPermissionsPolicyResponse"
    "fixture/DeleteDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainPermissionsPolicy)

responseGetPackageVersionReadme :: GetPackageVersionReadmeResponse -> TestTree
responseGetPackageVersionReadme =
  res
    "GetPackageVersionReadmeResponse"
    "fixture/GetPackageVersionReadmeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPackageVersionReadme)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseGetRepositoryPermissionsPolicy :: GetRepositoryPermissionsPolicyResponse -> TestTree
responseGetRepositoryPermissionsPolicy =
  res
    "GetRepositoryPermissionsPolicyResponse"
    "fixture/GetRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryPermissionsPolicy)

responseAssociateExternalConnection :: AssociateExternalConnectionResponse -> TestTree
responseAssociateExternalConnection =
  res
    "AssociateExternalConnectionResponse"
    "fixture/AssociateExternalConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateExternalConnection)

responseDescribeRepository :: DescribeRepositoryResponse -> TestTree
responseDescribeRepository =
  res
    "DescribeRepositoryResponse"
    "fixture/DescribeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepository)

responseDeletePackageVersions :: DeletePackageVersionsResponse -> TestTree
responseDeletePackageVersions =
  res
    "DeletePackageVersionsResponse"
    "fixture/DeletePackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackageVersions)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribePackageVersion :: DescribePackageVersionResponse -> TestTree
responseDescribePackageVersion =
  res
    "DescribePackageVersionResponse"
    "fixture/DescribePackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackageVersion)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseUpdateRepository :: UpdateRepositoryResponse -> TestTree
responseUpdateRepository =
  res
    "UpdateRepositoryResponse"
    "fixture/UpdateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepository)

responseCopyPackageVersions :: CopyPackageVersionsResponse -> TestTree
responseCopyPackageVersions =
  res
    "CopyPackageVersionsResponse"
    "fixture/CopyPackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyPackageVersions)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizationToken)

responseDisassociateExternalConnection :: DisassociateExternalConnectionResponse -> TestTree
responseDisassociateExternalConnection =
  res
    "DisassociateExternalConnectionResponse"
    "fixture/DisassociateExternalConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateExternalConnection)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListPackageVersions :: ListPackageVersionsResponse -> TestTree
responseListPackageVersions =
  res
    "ListPackageVersionsResponse"
    "fixture/ListPackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersions)

responseListPackageVersionAssets :: ListPackageVersionAssetsResponse -> TestTree
responseListPackageVersionAssets =
  res
    "ListPackageVersionAssetsResponse"
    "fixture/ListPackageVersionAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersionAssets)
