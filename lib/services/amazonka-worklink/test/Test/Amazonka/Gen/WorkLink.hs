{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WorkLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WorkLink where

import Amazonka.WorkLink
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WorkLink.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateDomainMetadata $
--             newUpdateDomainMetadata
--
--         , requestSignOutUser $
--             newSignOutUser
--
--         , requestRestoreDomainAccess $
--             newRestoreDomainAccess
--
--         , requestAssociateDomain $
--             newAssociateDomain
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDisassociateDomain $
--             newDisassociateDomain
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestAssociateWebsiteAuthorizationProvider $
--             newAssociateWebsiteAuthorizationProvider
--
--         , requestDescribeDevicePolicyConfiguration $
--             newDescribeDevicePolicyConfiguration
--
--         , requestListWebsiteAuthorizationProviders $
--             newListWebsiteAuthorizationProviders
--
--         , requestDisassociateWebsiteAuthorizationProvider $
--             newDisassociateWebsiteAuthorizationProvider
--
--         , requestListWebsiteCertificateAuthorities $
--             newListWebsiteCertificateAuthorities
--
--         , requestUpdateIdentityProviderConfiguration $
--             newUpdateIdentityProviderConfiguration
--
--         , requestDescribeFleetMetadata $
--             newDescribeFleetMetadata
--
--         , requestUpdateDevicePolicyConfiguration $
--             newUpdateDevicePolicyConfiguration
--
--         , requestDescribeCompanyNetworkConfiguration $
--             newDescribeCompanyNetworkConfiguration
--
--         , requestDescribeAuditStreamConfiguration $
--             newDescribeAuditStreamConfiguration
--
--         , requestRevokeDomainAccess $
--             newRevokeDomainAccess
--
--         , requestListFleets $
--             newListFleets
--
--         , requestUpdateFleetMetadata $
--             newUpdateFleetMetadata
--
--         , requestDescribeIdentityProviderConfiguration $
--             newDescribeIdentityProviderConfiguration
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDescribeWebsiteCertificateAuthority $
--             newDescribeWebsiteCertificateAuthority
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDisassociateWebsiteCertificateAuthority $
--             newDisassociateWebsiteCertificateAuthority
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestUpdateAuditStreamConfiguration $
--             newUpdateAuditStreamConfiguration
--
--         , requestUpdateCompanyNetworkConfiguration $
--             newUpdateCompanyNetworkConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAssociateWebsiteCertificateAuthority $
--             newAssociateWebsiteCertificateAuthority
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListDevices $
--             newListDevices
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDomainMetadata $
--             newUpdateDomainMetadataResponse
--
--         , responseSignOutUser $
--             newSignOutUserResponse
--
--         , responseRestoreDomainAccess $
--             newRestoreDomainAccessResponse
--
--         , responseAssociateDomain $
--             newAssociateDomainResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDisassociateDomain $
--             newDisassociateDomainResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseAssociateWebsiteAuthorizationProvider $
--             newAssociateWebsiteAuthorizationProviderResponse
--
--         , responseDescribeDevicePolicyConfiguration $
--             newDescribeDevicePolicyConfigurationResponse
--
--         , responseListWebsiteAuthorizationProviders $
--             newListWebsiteAuthorizationProvidersResponse
--
--         , responseDisassociateWebsiteAuthorizationProvider $
--             newDisassociateWebsiteAuthorizationProviderResponse
--
--         , responseListWebsiteCertificateAuthorities $
--             newListWebsiteCertificateAuthoritiesResponse
--
--         , responseUpdateIdentityProviderConfiguration $
--             newUpdateIdentityProviderConfigurationResponse
--
--         , responseDescribeFleetMetadata $
--             newDescribeFleetMetadataResponse
--
--         , responseUpdateDevicePolicyConfiguration $
--             newUpdateDevicePolicyConfigurationResponse
--
--         , responseDescribeCompanyNetworkConfiguration $
--             newDescribeCompanyNetworkConfigurationResponse
--
--         , responseDescribeAuditStreamConfiguration $
--             newDescribeAuditStreamConfigurationResponse
--
--         , responseRevokeDomainAccess $
--             newRevokeDomainAccessResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseUpdateFleetMetadata $
--             newUpdateFleetMetadataResponse
--
--         , responseDescribeIdentityProviderConfiguration $
--             newDescribeIdentityProviderConfigurationResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDescribeWebsiteCertificateAuthority $
--             newDescribeWebsiteCertificateAuthorityResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDisassociateWebsiteCertificateAuthority $
--             newDisassociateWebsiteCertificateAuthorityResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseUpdateAuditStreamConfiguration $
--             newUpdateAuditStreamConfigurationResponse
--
--         , responseUpdateCompanyNetworkConfiguration $
--             newUpdateCompanyNetworkConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAssociateWebsiteCertificateAuthority $
--             newAssociateWebsiteCertificateAuthorityResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--           ]
--     ]

-- Requests

requestUpdateDomainMetadata :: UpdateDomainMetadata -> TestTree
requestUpdateDomainMetadata =
  req
    "UpdateDomainMetadata"
    "fixture/UpdateDomainMetadata.yaml"

requestSignOutUser :: SignOutUser -> TestTree
requestSignOutUser =
  req
    "SignOutUser"
    "fixture/SignOutUser.yaml"

requestRestoreDomainAccess :: RestoreDomainAccess -> TestTree
requestRestoreDomainAccess =
  req
    "RestoreDomainAccess"
    "fixture/RestoreDomainAccess.yaml"

requestAssociateDomain :: AssociateDomain -> TestTree
requestAssociateDomain =
  req
    "AssociateDomain"
    "fixture/AssociateDomain.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDisassociateDomain :: DisassociateDomain -> TestTree
requestDisassociateDomain =
  req
    "DisassociateDomain"
    "fixture/DisassociateDomain.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestAssociateWebsiteAuthorizationProvider :: AssociateWebsiteAuthorizationProvider -> TestTree
requestAssociateWebsiteAuthorizationProvider =
  req
    "AssociateWebsiteAuthorizationProvider"
    "fixture/AssociateWebsiteAuthorizationProvider.yaml"

requestDescribeDevicePolicyConfiguration :: DescribeDevicePolicyConfiguration -> TestTree
requestDescribeDevicePolicyConfiguration =
  req
    "DescribeDevicePolicyConfiguration"
    "fixture/DescribeDevicePolicyConfiguration.yaml"

requestListWebsiteAuthorizationProviders :: ListWebsiteAuthorizationProviders -> TestTree
requestListWebsiteAuthorizationProviders =
  req
    "ListWebsiteAuthorizationProviders"
    "fixture/ListWebsiteAuthorizationProviders.yaml"

requestDisassociateWebsiteAuthorizationProvider :: DisassociateWebsiteAuthorizationProvider -> TestTree
requestDisassociateWebsiteAuthorizationProvider =
  req
    "DisassociateWebsiteAuthorizationProvider"
    "fixture/DisassociateWebsiteAuthorizationProvider.yaml"

requestListWebsiteCertificateAuthorities :: ListWebsiteCertificateAuthorities -> TestTree
requestListWebsiteCertificateAuthorities =
  req
    "ListWebsiteCertificateAuthorities"
    "fixture/ListWebsiteCertificateAuthorities.yaml"

requestUpdateIdentityProviderConfiguration :: UpdateIdentityProviderConfiguration -> TestTree
requestUpdateIdentityProviderConfiguration =
  req
    "UpdateIdentityProviderConfiguration"
    "fixture/UpdateIdentityProviderConfiguration.yaml"

requestDescribeFleetMetadata :: DescribeFleetMetadata -> TestTree
requestDescribeFleetMetadata =
  req
    "DescribeFleetMetadata"
    "fixture/DescribeFleetMetadata.yaml"

requestUpdateDevicePolicyConfiguration :: UpdateDevicePolicyConfiguration -> TestTree
requestUpdateDevicePolicyConfiguration =
  req
    "UpdateDevicePolicyConfiguration"
    "fixture/UpdateDevicePolicyConfiguration.yaml"

requestDescribeCompanyNetworkConfiguration :: DescribeCompanyNetworkConfiguration -> TestTree
requestDescribeCompanyNetworkConfiguration =
  req
    "DescribeCompanyNetworkConfiguration"
    "fixture/DescribeCompanyNetworkConfiguration.yaml"

requestDescribeAuditStreamConfiguration :: DescribeAuditStreamConfiguration -> TestTree
requestDescribeAuditStreamConfiguration =
  req
    "DescribeAuditStreamConfiguration"
    "fixture/DescribeAuditStreamConfiguration.yaml"

requestRevokeDomainAccess :: RevokeDomainAccess -> TestTree
requestRevokeDomainAccess =
  req
    "RevokeDomainAccess"
    "fixture/RevokeDomainAccess.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestUpdateFleetMetadata :: UpdateFleetMetadata -> TestTree
requestUpdateFleetMetadata =
  req
    "UpdateFleetMetadata"
    "fixture/UpdateFleetMetadata.yaml"

requestDescribeIdentityProviderConfiguration :: DescribeIdentityProviderConfiguration -> TestTree
requestDescribeIdentityProviderConfiguration =
  req
    "DescribeIdentityProviderConfiguration"
    "fixture/DescribeIdentityProviderConfiguration.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDescribeWebsiteCertificateAuthority :: DescribeWebsiteCertificateAuthority -> TestTree
requestDescribeWebsiteCertificateAuthority =
  req
    "DescribeWebsiteCertificateAuthority"
    "fixture/DescribeWebsiteCertificateAuthority.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDisassociateWebsiteCertificateAuthority :: DisassociateWebsiteCertificateAuthority -> TestTree
requestDisassociateWebsiteCertificateAuthority =
  req
    "DisassociateWebsiteCertificateAuthority"
    "fixture/DisassociateWebsiteCertificateAuthority.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestUpdateAuditStreamConfiguration :: UpdateAuditStreamConfiguration -> TestTree
requestUpdateAuditStreamConfiguration =
  req
    "UpdateAuditStreamConfiguration"
    "fixture/UpdateAuditStreamConfiguration.yaml"

requestUpdateCompanyNetworkConfiguration :: UpdateCompanyNetworkConfiguration -> TestTree
requestUpdateCompanyNetworkConfiguration =
  req
    "UpdateCompanyNetworkConfiguration"
    "fixture/UpdateCompanyNetworkConfiguration.yaml"

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

requestAssociateWebsiteCertificateAuthority :: AssociateWebsiteCertificateAuthority -> TestTree
requestAssociateWebsiteCertificateAuthority =
  req
    "AssociateWebsiteCertificateAuthority"
    "fixture/AssociateWebsiteCertificateAuthority.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

-- Responses

responseUpdateDomainMetadata :: UpdateDomainMetadataResponse -> TestTree
responseUpdateDomainMetadata =
  res
    "UpdateDomainMetadataResponse"
    "fixture/UpdateDomainMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainMetadata)

responseSignOutUser :: SignOutUserResponse -> TestTree
responseSignOutUser =
  res
    "SignOutUserResponse"
    "fixture/SignOutUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignOutUser)

responseRestoreDomainAccess :: RestoreDomainAccessResponse -> TestTree
responseRestoreDomainAccess =
  res
    "RestoreDomainAccessResponse"
    "fixture/RestoreDomainAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDomainAccess)

responseAssociateDomain :: AssociateDomainResponse -> TestTree
responseAssociateDomain =
  res
    "AssociateDomainResponse"
    "fixture/AssociateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDomain)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDisassociateDomain :: DisassociateDomainResponse -> TestTree
responseDisassociateDomain =
  res
    "DisassociateDomainResponse"
    "fixture/DisassociateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDomain)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseAssociateWebsiteAuthorizationProvider :: AssociateWebsiteAuthorizationProviderResponse -> TestTree
responseAssociateWebsiteAuthorizationProvider =
  res
    "AssociateWebsiteAuthorizationProviderResponse"
    "fixture/AssociateWebsiteAuthorizationProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebsiteAuthorizationProvider)

responseDescribeDevicePolicyConfiguration :: DescribeDevicePolicyConfigurationResponse -> TestTree
responseDescribeDevicePolicyConfiguration =
  res
    "DescribeDevicePolicyConfigurationResponse"
    "fixture/DescribeDevicePolicyConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevicePolicyConfiguration)

responseListWebsiteAuthorizationProviders :: ListWebsiteAuthorizationProvidersResponse -> TestTree
responseListWebsiteAuthorizationProviders =
  res
    "ListWebsiteAuthorizationProvidersResponse"
    "fixture/ListWebsiteAuthorizationProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebsiteAuthorizationProviders)

responseDisassociateWebsiteAuthorizationProvider :: DisassociateWebsiteAuthorizationProviderResponse -> TestTree
responseDisassociateWebsiteAuthorizationProvider =
  res
    "DisassociateWebsiteAuthorizationProviderResponse"
    "fixture/DisassociateWebsiteAuthorizationProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebsiteAuthorizationProvider)

responseListWebsiteCertificateAuthorities :: ListWebsiteCertificateAuthoritiesResponse -> TestTree
responseListWebsiteCertificateAuthorities =
  res
    "ListWebsiteCertificateAuthoritiesResponse"
    "fixture/ListWebsiteCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebsiteCertificateAuthorities)

responseUpdateIdentityProviderConfiguration :: UpdateIdentityProviderConfigurationResponse -> TestTree
responseUpdateIdentityProviderConfiguration =
  res
    "UpdateIdentityProviderConfigurationResponse"
    "fixture/UpdateIdentityProviderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProviderConfiguration)

responseDescribeFleetMetadata :: DescribeFleetMetadataResponse -> TestTree
responseDescribeFleetMetadata =
  res
    "DescribeFleetMetadataResponse"
    "fixture/DescribeFleetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetMetadata)

responseUpdateDevicePolicyConfiguration :: UpdateDevicePolicyConfigurationResponse -> TestTree
responseUpdateDevicePolicyConfiguration =
  res
    "UpdateDevicePolicyConfigurationResponse"
    "fixture/UpdateDevicePolicyConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevicePolicyConfiguration)

responseDescribeCompanyNetworkConfiguration :: DescribeCompanyNetworkConfigurationResponse -> TestTree
responseDescribeCompanyNetworkConfiguration =
  res
    "DescribeCompanyNetworkConfigurationResponse"
    "fixture/DescribeCompanyNetworkConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCompanyNetworkConfiguration)

responseDescribeAuditStreamConfiguration :: DescribeAuditStreamConfigurationResponse -> TestTree
responseDescribeAuditStreamConfiguration =
  res
    "DescribeAuditStreamConfigurationResponse"
    "fixture/DescribeAuditStreamConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditStreamConfiguration)

responseRevokeDomainAccess :: RevokeDomainAccessResponse -> TestTree
responseRevokeDomainAccess =
  res
    "RevokeDomainAccessResponse"
    "fixture/RevokeDomainAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeDomainAccess)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseUpdateFleetMetadata :: UpdateFleetMetadataResponse -> TestTree
responseUpdateFleetMetadata =
  res
    "UpdateFleetMetadataResponse"
    "fixture/UpdateFleetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetMetadata)

responseDescribeIdentityProviderConfiguration :: DescribeIdentityProviderConfigurationResponse -> TestTree
responseDescribeIdentityProviderConfiguration =
  res
    "DescribeIdentityProviderConfigurationResponse"
    "fixture/DescribeIdentityProviderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityProviderConfiguration)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseDescribeWebsiteCertificateAuthority :: DescribeWebsiteCertificateAuthorityResponse -> TestTree
responseDescribeWebsiteCertificateAuthority =
  res
    "DescribeWebsiteCertificateAuthorityResponse"
    "fixture/DescribeWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWebsiteCertificateAuthority)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDisassociateWebsiteCertificateAuthority :: DisassociateWebsiteCertificateAuthorityResponse -> TestTree
responseDisassociateWebsiteCertificateAuthority =
  res
    "DisassociateWebsiteCertificateAuthorityResponse"
    "fixture/DisassociateWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebsiteCertificateAuthority)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseUpdateAuditStreamConfiguration :: UpdateAuditStreamConfigurationResponse -> TestTree
responseUpdateAuditStreamConfiguration =
  res
    "UpdateAuditStreamConfigurationResponse"
    "fixture/UpdateAuditStreamConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuditStreamConfiguration)

responseUpdateCompanyNetworkConfiguration :: UpdateCompanyNetworkConfigurationResponse -> TestTree
responseUpdateCompanyNetworkConfiguration =
  res
    "UpdateCompanyNetworkConfigurationResponse"
    "fixture/UpdateCompanyNetworkConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCompanyNetworkConfiguration)

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

responseAssociateWebsiteCertificateAuthority :: AssociateWebsiteCertificateAuthorityResponse -> TestTree
responseAssociateWebsiteCertificateAuthority =
  res
    "AssociateWebsiteCertificateAuthorityResponse"
    "fixture/AssociateWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebsiteCertificateAuthority)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)
