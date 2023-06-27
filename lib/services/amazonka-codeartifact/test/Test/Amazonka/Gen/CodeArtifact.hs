{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeArtifact where

import Amazonka.CodeArtifact
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeArtifact.Internal
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
--         [ requestAssociateExternalConnection $
--             newAssociateExternalConnection
--
--         , requestCopyPackageVersions $
--             newCopyPackageVersions
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteDomainPermissionsPolicy $
--             newDeleteDomainPermissionsPolicy
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestDeletePackageVersions $
--             newDeletePackageVersions
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteRepositoryPermissionsPolicy $
--             newDeleteRepositoryPermissionsPolicy
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribePackage $
--             newDescribePackage
--
--         , requestDescribePackageVersion $
--             newDescribePackageVersion
--
--         , requestDescribeRepository $
--             newDescribeRepository
--
--         , requestDisassociateExternalConnection $
--             newDisassociateExternalConnection
--
--         , requestDisposePackageVersions $
--             newDisposePackageVersions
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestGetDomainPermissionsPolicy $
--             newGetDomainPermissionsPolicy
--
--         , requestGetPackageVersionAsset $
--             newGetPackageVersionAsset
--
--         , requestGetPackageVersionReadme $
--             newGetPackageVersionReadme
--
--         , requestGetRepositoryEndpoint $
--             newGetRepositoryEndpoint
--
--         , requestGetRepositoryPermissionsPolicy $
--             newGetRepositoryPermissionsPolicy
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListPackageVersionAssets $
--             newListPackageVersionAssets
--
--         , requestListPackageVersionDependencies $
--             newListPackageVersionDependencies
--
--         , requestListPackageVersions $
--             newListPackageVersions
--
--         , requestListPackages $
--             newListPackages
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestListRepositoriesInDomain $
--             newListRepositoriesInDomain
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPublishPackageVersion $
--             newPublishPackageVersion
--
--         , requestPutDomainPermissionsPolicy $
--             newPutDomainPermissionsPolicy
--
--         , requestPutPackageOriginConfiguration $
--             newPutPackageOriginConfiguration
--
--         , requestPutRepositoryPermissionsPolicy $
--             newPutRepositoryPermissionsPolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePackageVersionsStatus $
--             newUpdatePackageVersionsStatus
--
--         , requestUpdateRepository $
--             newUpdateRepository
--
--           ]

--     , testGroup "response"
--         [ responseAssociateExternalConnection $
--             newAssociateExternalConnectionResponse
--
--         , responseCopyPackageVersions $
--             newCopyPackageVersionsResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteDomainPermissionsPolicy $
--             newDeleteDomainPermissionsPolicyResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseDeletePackageVersions $
--             newDeletePackageVersionsResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteRepositoryPermissionsPolicy $
--             newDeleteRepositoryPermissionsPolicyResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribePackage $
--             newDescribePackageResponse
--
--         , responseDescribePackageVersion $
--             newDescribePackageVersionResponse
--
--         , responseDescribeRepository $
--             newDescribeRepositoryResponse
--
--         , responseDisassociateExternalConnection $
--             newDisassociateExternalConnectionResponse
--
--         , responseDisposePackageVersions $
--             newDisposePackageVersionsResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseGetDomainPermissionsPolicy $
--             newGetDomainPermissionsPolicyResponse
--
--         , responseGetPackageVersionAsset $
--             newGetPackageVersionAssetResponse
--
--         , responseGetPackageVersionReadme $
--             newGetPackageVersionReadmeResponse
--
--         , responseGetRepositoryEndpoint $
--             newGetRepositoryEndpointResponse
--
--         , responseGetRepositoryPermissionsPolicy $
--             newGetRepositoryPermissionsPolicyResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListPackageVersionAssets $
--             newListPackageVersionAssetsResponse
--
--         , responseListPackageVersionDependencies $
--             newListPackageVersionDependenciesResponse
--
--         , responseListPackageVersions $
--             newListPackageVersionsResponse
--
--         , responseListPackages $
--             newListPackagesResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseListRepositoriesInDomain $
--             newListRepositoriesInDomainResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePublishPackageVersion $
--             newPublishPackageVersionResponse
--
--         , responsePutDomainPermissionsPolicy $
--             newPutDomainPermissionsPolicyResponse
--
--         , responsePutPackageOriginConfiguration $
--             newPutPackageOriginConfigurationResponse
--
--         , responsePutRepositoryPermissionsPolicy $
--             newPutRepositoryPermissionsPolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePackageVersionsStatus $
--             newUpdatePackageVersionsStatusResponse
--
--         , responseUpdateRepository $
--             newUpdateRepositoryResponse
--
--           ]
--     ]

-- Requests

requestAssociateExternalConnection :: AssociateExternalConnection -> TestTree
requestAssociateExternalConnection =
  req
    "AssociateExternalConnection"
    "fixture/AssociateExternalConnection.yaml"

requestCopyPackageVersions :: CopyPackageVersions -> TestTree
requestCopyPackageVersions =
  req
    "CopyPackageVersions"
    "fixture/CopyPackageVersions.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteDomainPermissionsPolicy :: DeleteDomainPermissionsPolicy -> TestTree
requestDeleteDomainPermissionsPolicy =
  req
    "DeleteDomainPermissionsPolicy"
    "fixture/DeleteDomainPermissionsPolicy.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestDeletePackageVersions :: DeletePackageVersions -> TestTree
requestDeletePackageVersions =
  req
    "DeletePackageVersions"
    "fixture/DeletePackageVersions.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDeleteRepositoryPermissionsPolicy :: DeleteRepositoryPermissionsPolicy -> TestTree
requestDeleteRepositoryPermissionsPolicy =
  req
    "DeleteRepositoryPermissionsPolicy"
    "fixture/DeleteRepositoryPermissionsPolicy.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribePackage :: DescribePackage -> TestTree
requestDescribePackage =
  req
    "DescribePackage"
    "fixture/DescribePackage.yaml"

requestDescribePackageVersion :: DescribePackageVersion -> TestTree
requestDescribePackageVersion =
  req
    "DescribePackageVersion"
    "fixture/DescribePackageVersion.yaml"

requestDescribeRepository :: DescribeRepository -> TestTree
requestDescribeRepository =
  req
    "DescribeRepository"
    "fixture/DescribeRepository.yaml"

requestDisassociateExternalConnection :: DisassociateExternalConnection -> TestTree
requestDisassociateExternalConnection =
  req
    "DisassociateExternalConnection"
    "fixture/DisassociateExternalConnection.yaml"

requestDisposePackageVersions :: DisposePackageVersions -> TestTree
requestDisposePackageVersions =
  req
    "DisposePackageVersions"
    "fixture/DisposePackageVersions.yaml"

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken =
  req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

requestGetDomainPermissionsPolicy :: GetDomainPermissionsPolicy -> TestTree
requestGetDomainPermissionsPolicy =
  req
    "GetDomainPermissionsPolicy"
    "fixture/GetDomainPermissionsPolicy.yaml"

requestGetPackageVersionAsset :: GetPackageVersionAsset -> TestTree
requestGetPackageVersionAsset =
  req
    "GetPackageVersionAsset"
    "fixture/GetPackageVersionAsset.yaml"

requestGetPackageVersionReadme :: GetPackageVersionReadme -> TestTree
requestGetPackageVersionReadme =
  req
    "GetPackageVersionReadme"
    "fixture/GetPackageVersionReadme.yaml"

requestGetRepositoryEndpoint :: GetRepositoryEndpoint -> TestTree
requestGetRepositoryEndpoint =
  req
    "GetRepositoryEndpoint"
    "fixture/GetRepositoryEndpoint.yaml"

requestGetRepositoryPermissionsPolicy :: GetRepositoryPermissionsPolicy -> TestTree
requestGetRepositoryPermissionsPolicy =
  req
    "GetRepositoryPermissionsPolicy"
    "fixture/GetRepositoryPermissionsPolicy.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListPackageVersionAssets :: ListPackageVersionAssets -> TestTree
requestListPackageVersionAssets =
  req
    "ListPackageVersionAssets"
    "fixture/ListPackageVersionAssets.yaml"

requestListPackageVersionDependencies :: ListPackageVersionDependencies -> TestTree
requestListPackageVersionDependencies =
  req
    "ListPackageVersionDependencies"
    "fixture/ListPackageVersionDependencies.yaml"

requestListPackageVersions :: ListPackageVersions -> TestTree
requestListPackageVersions =
  req
    "ListPackageVersions"
    "fixture/ListPackageVersions.yaml"

requestListPackages :: ListPackages -> TestTree
requestListPackages =
  req
    "ListPackages"
    "fixture/ListPackages.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestListRepositoriesInDomain :: ListRepositoriesInDomain -> TestTree
requestListRepositoriesInDomain =
  req
    "ListRepositoriesInDomain"
    "fixture/ListRepositoriesInDomain.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutDomainPermissionsPolicy :: PutDomainPermissionsPolicy -> TestTree
requestPutDomainPermissionsPolicy =
  req
    "PutDomainPermissionsPolicy"
    "fixture/PutDomainPermissionsPolicy.yaml"

requestPutPackageOriginConfiguration :: PutPackageOriginConfiguration -> TestTree
requestPutPackageOriginConfiguration =
  req
    "PutPackageOriginConfiguration"
    "fixture/PutPackageOriginConfiguration.yaml"

requestPutRepositoryPermissionsPolicy :: PutRepositoryPermissionsPolicy -> TestTree
requestPutRepositoryPermissionsPolicy =
  req
    "PutRepositoryPermissionsPolicy"
    "fixture/PutRepositoryPermissionsPolicy.yaml"

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

requestUpdatePackageVersionsStatus :: UpdatePackageVersionsStatus -> TestTree
requestUpdatePackageVersionsStatus =
  req
    "UpdatePackageVersionsStatus"
    "fixture/UpdatePackageVersionsStatus.yaml"

requestUpdateRepository :: UpdateRepository -> TestTree
requestUpdateRepository =
  req
    "UpdateRepository"
    "fixture/UpdateRepository.yaml"

-- Responses

responseAssociateExternalConnection :: AssociateExternalConnectionResponse -> TestTree
responseAssociateExternalConnection =
  res
    "AssociateExternalConnectionResponse"
    "fixture/AssociateExternalConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateExternalConnection)

responseCopyPackageVersions :: CopyPackageVersionsResponse -> TestTree
responseCopyPackageVersions =
  res
    "CopyPackageVersionsResponse"
    "fixture/CopyPackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyPackageVersions)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteDomainPermissionsPolicy :: DeleteDomainPermissionsPolicyResponse -> TestTree
responseDeleteDomainPermissionsPolicy =
  res
    "DeleteDomainPermissionsPolicyResponse"
    "fixture/DeleteDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainPermissionsPolicy)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackage)

responseDeletePackageVersions :: DeletePackageVersionsResponse -> TestTree
responseDeletePackageVersions =
  res
    "DeletePackageVersionsResponse"
    "fixture/DeletePackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackageVersions)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseDeleteRepositoryPermissionsPolicy :: DeleteRepositoryPermissionsPolicyResponse -> TestTree
responseDeleteRepositoryPermissionsPolicy =
  res
    "DeleteRepositoryPermissionsPolicyResponse"
    "fixture/DeleteRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepositoryPermissionsPolicy)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribePackage :: DescribePackageResponse -> TestTree
responseDescribePackage =
  res
    "DescribePackageResponse"
    "fixture/DescribePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackage)

responseDescribePackageVersion :: DescribePackageVersionResponse -> TestTree
responseDescribePackageVersion =
  res
    "DescribePackageVersionResponse"
    "fixture/DescribePackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackageVersion)

responseDescribeRepository :: DescribeRepositoryResponse -> TestTree
responseDescribeRepository =
  res
    "DescribeRepositoryResponse"
    "fixture/DescribeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepository)

responseDisassociateExternalConnection :: DisassociateExternalConnectionResponse -> TestTree
responseDisassociateExternalConnection =
  res
    "DisassociateExternalConnectionResponse"
    "fixture/DisassociateExternalConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateExternalConnection)

responseDisposePackageVersions :: DisposePackageVersionsResponse -> TestTree
responseDisposePackageVersions =
  res
    "DisposePackageVersionsResponse"
    "fixture/DisposePackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisposePackageVersions)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizationToken)

responseGetDomainPermissionsPolicy :: GetDomainPermissionsPolicyResponse -> TestTree
responseGetDomainPermissionsPolicy =
  res
    "GetDomainPermissionsPolicyResponse"
    "fixture/GetDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainPermissionsPolicy)

responseGetPackageVersionReadme :: GetPackageVersionReadmeResponse -> TestTree
responseGetPackageVersionReadme =
  res
    "GetPackageVersionReadmeResponse"
    "fixture/GetPackageVersionReadmeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPackageVersionReadme)

responseGetRepositoryEndpoint :: GetRepositoryEndpointResponse -> TestTree
responseGetRepositoryEndpoint =
  res
    "GetRepositoryEndpointResponse"
    "fixture/GetRepositoryEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryEndpoint)

responseGetRepositoryPermissionsPolicy :: GetRepositoryPermissionsPolicyResponse -> TestTree
responseGetRepositoryPermissionsPolicy =
  res
    "GetRepositoryPermissionsPolicyResponse"
    "fixture/GetRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryPermissionsPolicy)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListPackageVersionAssets :: ListPackageVersionAssetsResponse -> TestTree
responseListPackageVersionAssets =
  res
    "ListPackageVersionAssetsResponse"
    "fixture/ListPackageVersionAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersionAssets)

responseListPackageVersionDependencies :: ListPackageVersionDependenciesResponse -> TestTree
responseListPackageVersionDependencies =
  res
    "ListPackageVersionDependenciesResponse"
    "fixture/ListPackageVersionDependenciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersionDependencies)

responseListPackageVersions :: ListPackageVersionsResponse -> TestTree
responseListPackageVersions =
  res
    "ListPackageVersionsResponse"
    "fixture/ListPackageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageVersions)

responseListPackages :: ListPackagesResponse -> TestTree
responseListPackages =
  res
    "ListPackagesResponse"
    "fixture/ListPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackages)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositories)

responseListRepositoriesInDomain :: ListRepositoriesInDomainResponse -> TestTree
responseListRepositoriesInDomain =
  res
    "ListRepositoriesInDomainResponse"
    "fixture/ListRepositoriesInDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoriesInDomain)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePublishPackageVersion :: PublishPackageVersionResponse -> TestTree
responsePublishPackageVersion =
  res
    "PublishPackageVersionResponse"
    "fixture/PublishPackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishPackageVersion)

responsePutDomainPermissionsPolicy :: PutDomainPermissionsPolicyResponse -> TestTree
responsePutDomainPermissionsPolicy =
  res
    "PutDomainPermissionsPolicyResponse"
    "fixture/PutDomainPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDomainPermissionsPolicy)

responsePutPackageOriginConfiguration :: PutPackageOriginConfigurationResponse -> TestTree
responsePutPackageOriginConfiguration =
  res
    "PutPackageOriginConfigurationResponse"
    "fixture/PutPackageOriginConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPackageOriginConfiguration)

responsePutRepositoryPermissionsPolicy :: PutRepositoryPermissionsPolicyResponse -> TestTree
responsePutRepositoryPermissionsPolicy =
  res
    "PutRepositoryPermissionsPolicyResponse"
    "fixture/PutRepositoryPermissionsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryPermissionsPolicy)

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

responseUpdatePackageVersionsStatus :: UpdatePackageVersionsStatusResponse -> TestTree
responseUpdatePackageVersionsStatus =
  res
    "UpdatePackageVersionsStatusResponse"
    "fixture/UpdatePackageVersionsStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePackageVersionsStatus)

responseUpdateRepository :: UpdateRepositoryResponse -> TestTree
responseUpdateRepository =
  res
    "UpdateRepositoryResponse"
    "fixture/UpdateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepository)
