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
--         [ requestAssociateDomain $
--             newAssociateDomain
--
--         , requestAssociateWebsiteAuthorizationProvider $
--             newAssociateWebsiteAuthorizationProvider
--
--         , requestAssociateWebsiteCertificateAuthority $
--             newAssociateWebsiteCertificateAuthority
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDescribeAuditStreamConfiguration $
--             newDescribeAuditStreamConfiguration
--
--         , requestDescribeCompanyNetworkConfiguration $
--             newDescribeCompanyNetworkConfiguration
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribeDevicePolicyConfiguration $
--             newDescribeDevicePolicyConfiguration
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribeFleetMetadata $
--             newDescribeFleetMetadata
--
--         , requestDescribeIdentityProviderConfiguration $
--             newDescribeIdentityProviderConfiguration
--
--         , requestDescribeWebsiteCertificateAuthority $
--             newDescribeWebsiteCertificateAuthority
--
--         , requestDisassociateDomain $
--             newDisassociateDomain
--
--         , requestDisassociateWebsiteAuthorizationProvider $
--             newDisassociateWebsiteAuthorizationProvider
--
--         , requestDisassociateWebsiteCertificateAuthority $
--             newDisassociateWebsiteCertificateAuthority
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListFleets $
--             newListFleets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebsiteAuthorizationProviders $
--             newListWebsiteAuthorizationProviders
--
--         , requestListWebsiteCertificateAuthorities $
--             newListWebsiteCertificateAuthorities
--
--         , requestRestoreDomainAccess $
--             newRestoreDomainAccess
--
--         , requestRevokeDomainAccess $
--             newRevokeDomainAccess
--
--         , requestSignOutUser $
--             newSignOutUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAuditStreamConfiguration $
--             newUpdateAuditStreamConfiguration
--
--         , requestUpdateCompanyNetworkConfiguration $
--             newUpdateCompanyNetworkConfiguration
--
--         , requestUpdateDevicePolicyConfiguration $
--             newUpdateDevicePolicyConfiguration
--
--         , requestUpdateDomainMetadata $
--             newUpdateDomainMetadata
--
--         , requestUpdateFleetMetadata $
--             newUpdateFleetMetadata
--
--         , requestUpdateIdentityProviderConfiguration $
--             newUpdateIdentityProviderConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDomain $
--             newAssociateDomainResponse
--
--         , responseAssociateWebsiteAuthorizationProvider $
--             newAssociateWebsiteAuthorizationProviderResponse
--
--         , responseAssociateWebsiteCertificateAuthority $
--             newAssociateWebsiteCertificateAuthorityResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDescribeAuditStreamConfiguration $
--             newDescribeAuditStreamConfigurationResponse
--
--         , responseDescribeCompanyNetworkConfiguration $
--             newDescribeCompanyNetworkConfigurationResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribeDevicePolicyConfiguration $
--             newDescribeDevicePolicyConfigurationResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribeFleetMetadata $
--             newDescribeFleetMetadataResponse
--
--         , responseDescribeIdentityProviderConfiguration $
--             newDescribeIdentityProviderConfigurationResponse
--
--         , responseDescribeWebsiteCertificateAuthority $
--             newDescribeWebsiteCertificateAuthorityResponse
--
--         , responseDisassociateDomain $
--             newDisassociateDomainResponse
--
--         , responseDisassociateWebsiteAuthorizationProvider $
--             newDisassociateWebsiteAuthorizationProviderResponse
--
--         , responseDisassociateWebsiteCertificateAuthority $
--             newDisassociateWebsiteCertificateAuthorityResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebsiteAuthorizationProviders $
--             newListWebsiteAuthorizationProvidersResponse
--
--         , responseListWebsiteCertificateAuthorities $
--             newListWebsiteCertificateAuthoritiesResponse
--
--         , responseRestoreDomainAccess $
--             newRestoreDomainAccessResponse
--
--         , responseRevokeDomainAccess $
--             newRevokeDomainAccessResponse
--
--         , responseSignOutUser $
--             newSignOutUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAuditStreamConfiguration $
--             newUpdateAuditStreamConfigurationResponse
--
--         , responseUpdateCompanyNetworkConfiguration $
--             newUpdateCompanyNetworkConfigurationResponse
--
--         , responseUpdateDevicePolicyConfiguration $
--             newUpdateDevicePolicyConfigurationResponse
--
--         , responseUpdateDomainMetadata $
--             newUpdateDomainMetadataResponse
--
--         , responseUpdateFleetMetadata $
--             newUpdateFleetMetadataResponse
--
--         , responseUpdateIdentityProviderConfiguration $
--             newUpdateIdentityProviderConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAssociateDomain :: AssociateDomain -> TestTree
requestAssociateDomain =
  req
    "AssociateDomain"
    "fixture/AssociateDomain.yaml"

requestAssociateWebsiteAuthorizationProvider :: AssociateWebsiteAuthorizationProvider -> TestTree
requestAssociateWebsiteAuthorizationProvider =
  req
    "AssociateWebsiteAuthorizationProvider"
    "fixture/AssociateWebsiteAuthorizationProvider.yaml"

requestAssociateWebsiteCertificateAuthority :: AssociateWebsiteCertificateAuthority -> TestTree
requestAssociateWebsiteCertificateAuthority =
  req
    "AssociateWebsiteCertificateAuthority"
    "fixture/AssociateWebsiteCertificateAuthority.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDescribeAuditStreamConfiguration :: DescribeAuditStreamConfiguration -> TestTree
requestDescribeAuditStreamConfiguration =
  req
    "DescribeAuditStreamConfiguration"
    "fixture/DescribeAuditStreamConfiguration.yaml"

requestDescribeCompanyNetworkConfiguration :: DescribeCompanyNetworkConfiguration -> TestTree
requestDescribeCompanyNetworkConfiguration =
  req
    "DescribeCompanyNetworkConfiguration"
    "fixture/DescribeCompanyNetworkConfiguration.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribeDevicePolicyConfiguration :: DescribeDevicePolicyConfiguration -> TestTree
requestDescribeDevicePolicyConfiguration =
  req
    "DescribeDevicePolicyConfiguration"
    "fixture/DescribeDevicePolicyConfiguration.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribeFleetMetadata :: DescribeFleetMetadata -> TestTree
requestDescribeFleetMetadata =
  req
    "DescribeFleetMetadata"
    "fixture/DescribeFleetMetadata.yaml"

requestDescribeIdentityProviderConfiguration :: DescribeIdentityProviderConfiguration -> TestTree
requestDescribeIdentityProviderConfiguration =
  req
    "DescribeIdentityProviderConfiguration"
    "fixture/DescribeIdentityProviderConfiguration.yaml"

requestDescribeWebsiteCertificateAuthority :: DescribeWebsiteCertificateAuthority -> TestTree
requestDescribeWebsiteCertificateAuthority =
  req
    "DescribeWebsiteCertificateAuthority"
    "fixture/DescribeWebsiteCertificateAuthority.yaml"

requestDisassociateDomain :: DisassociateDomain -> TestTree
requestDisassociateDomain =
  req
    "DisassociateDomain"
    "fixture/DisassociateDomain.yaml"

requestDisassociateWebsiteAuthorizationProvider :: DisassociateWebsiteAuthorizationProvider -> TestTree
requestDisassociateWebsiteAuthorizationProvider =
  req
    "DisassociateWebsiteAuthorizationProvider"
    "fixture/DisassociateWebsiteAuthorizationProvider.yaml"

requestDisassociateWebsiteCertificateAuthority :: DisassociateWebsiteCertificateAuthority -> TestTree
requestDisassociateWebsiteCertificateAuthority =
  req
    "DisassociateWebsiteCertificateAuthority"
    "fixture/DisassociateWebsiteCertificateAuthority.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWebsiteAuthorizationProviders :: ListWebsiteAuthorizationProviders -> TestTree
requestListWebsiteAuthorizationProviders =
  req
    "ListWebsiteAuthorizationProviders"
    "fixture/ListWebsiteAuthorizationProviders.yaml"

requestListWebsiteCertificateAuthorities :: ListWebsiteCertificateAuthorities -> TestTree
requestListWebsiteCertificateAuthorities =
  req
    "ListWebsiteCertificateAuthorities"
    "fixture/ListWebsiteCertificateAuthorities.yaml"

requestRestoreDomainAccess :: RestoreDomainAccess -> TestTree
requestRestoreDomainAccess =
  req
    "RestoreDomainAccess"
    "fixture/RestoreDomainAccess.yaml"

requestRevokeDomainAccess :: RevokeDomainAccess -> TestTree
requestRevokeDomainAccess =
  req
    "RevokeDomainAccess"
    "fixture/RevokeDomainAccess.yaml"

requestSignOutUser :: SignOutUser -> TestTree
requestSignOutUser =
  req
    "SignOutUser"
    "fixture/SignOutUser.yaml"

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

requestUpdateDevicePolicyConfiguration :: UpdateDevicePolicyConfiguration -> TestTree
requestUpdateDevicePolicyConfiguration =
  req
    "UpdateDevicePolicyConfiguration"
    "fixture/UpdateDevicePolicyConfiguration.yaml"

requestUpdateDomainMetadata :: UpdateDomainMetadata -> TestTree
requestUpdateDomainMetadata =
  req
    "UpdateDomainMetadata"
    "fixture/UpdateDomainMetadata.yaml"

requestUpdateFleetMetadata :: UpdateFleetMetadata -> TestTree
requestUpdateFleetMetadata =
  req
    "UpdateFleetMetadata"
    "fixture/UpdateFleetMetadata.yaml"

requestUpdateIdentityProviderConfiguration :: UpdateIdentityProviderConfiguration -> TestTree
requestUpdateIdentityProviderConfiguration =
  req
    "UpdateIdentityProviderConfiguration"
    "fixture/UpdateIdentityProviderConfiguration.yaml"

-- Responses

responseAssociateDomain :: AssociateDomainResponse -> TestTree
responseAssociateDomain =
  res
    "AssociateDomainResponse"
    "fixture/AssociateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDomain)

responseAssociateWebsiteAuthorizationProvider :: AssociateWebsiteAuthorizationProviderResponse -> TestTree
responseAssociateWebsiteAuthorizationProvider =
  res
    "AssociateWebsiteAuthorizationProviderResponse"
    "fixture/AssociateWebsiteAuthorizationProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebsiteAuthorizationProvider)

responseAssociateWebsiteCertificateAuthority :: AssociateWebsiteCertificateAuthorityResponse -> TestTree
responseAssociateWebsiteCertificateAuthority =
  res
    "AssociateWebsiteCertificateAuthorityResponse"
    "fixture/AssociateWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebsiteCertificateAuthority)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDescribeAuditStreamConfiguration :: DescribeAuditStreamConfigurationResponse -> TestTree
responseDescribeAuditStreamConfiguration =
  res
    "DescribeAuditStreamConfigurationResponse"
    "fixture/DescribeAuditStreamConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuditStreamConfiguration)

responseDescribeCompanyNetworkConfiguration :: DescribeCompanyNetworkConfigurationResponse -> TestTree
responseDescribeCompanyNetworkConfiguration =
  res
    "DescribeCompanyNetworkConfigurationResponse"
    "fixture/DescribeCompanyNetworkConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCompanyNetworkConfiguration)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseDescribeDevicePolicyConfiguration :: DescribeDevicePolicyConfigurationResponse -> TestTree
responseDescribeDevicePolicyConfiguration =
  res
    "DescribeDevicePolicyConfigurationResponse"
    "fixture/DescribeDevicePolicyConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevicePolicyConfiguration)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribeFleetMetadata :: DescribeFleetMetadataResponse -> TestTree
responseDescribeFleetMetadata =
  res
    "DescribeFleetMetadataResponse"
    "fixture/DescribeFleetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetMetadata)

responseDescribeIdentityProviderConfiguration :: DescribeIdentityProviderConfigurationResponse -> TestTree
responseDescribeIdentityProviderConfiguration =
  res
    "DescribeIdentityProviderConfigurationResponse"
    "fixture/DescribeIdentityProviderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityProviderConfiguration)

responseDescribeWebsiteCertificateAuthority :: DescribeWebsiteCertificateAuthorityResponse -> TestTree
responseDescribeWebsiteCertificateAuthority =
  res
    "DescribeWebsiteCertificateAuthorityResponse"
    "fixture/DescribeWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWebsiteCertificateAuthority)

responseDisassociateDomain :: DisassociateDomainResponse -> TestTree
responseDisassociateDomain =
  res
    "DisassociateDomainResponse"
    "fixture/DisassociateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDomain)

responseDisassociateWebsiteAuthorizationProvider :: DisassociateWebsiteAuthorizationProviderResponse -> TestTree
responseDisassociateWebsiteAuthorizationProvider =
  res
    "DisassociateWebsiteAuthorizationProviderResponse"
    "fixture/DisassociateWebsiteAuthorizationProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebsiteAuthorizationProvider)

responseDisassociateWebsiteCertificateAuthority :: DisassociateWebsiteCertificateAuthorityResponse -> TestTree
responseDisassociateWebsiteCertificateAuthority =
  res
    "DisassociateWebsiteCertificateAuthorityResponse"
    "fixture/DisassociateWebsiteCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebsiteCertificateAuthority)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWebsiteAuthorizationProviders :: ListWebsiteAuthorizationProvidersResponse -> TestTree
responseListWebsiteAuthorizationProviders =
  res
    "ListWebsiteAuthorizationProvidersResponse"
    "fixture/ListWebsiteAuthorizationProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebsiteAuthorizationProviders)

responseListWebsiteCertificateAuthorities :: ListWebsiteCertificateAuthoritiesResponse -> TestTree
responseListWebsiteCertificateAuthorities =
  res
    "ListWebsiteCertificateAuthoritiesResponse"
    "fixture/ListWebsiteCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebsiteCertificateAuthorities)

responseRestoreDomainAccess :: RestoreDomainAccessResponse -> TestTree
responseRestoreDomainAccess =
  res
    "RestoreDomainAccessResponse"
    "fixture/RestoreDomainAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDomainAccess)

responseRevokeDomainAccess :: RevokeDomainAccessResponse -> TestTree
responseRevokeDomainAccess =
  res
    "RevokeDomainAccessResponse"
    "fixture/RevokeDomainAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeDomainAccess)

responseSignOutUser :: SignOutUserResponse -> TestTree
responseSignOutUser =
  res
    "SignOutUserResponse"
    "fixture/SignOutUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignOutUser)

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

responseUpdateDevicePolicyConfiguration :: UpdateDevicePolicyConfigurationResponse -> TestTree
responseUpdateDevicePolicyConfiguration =
  res
    "UpdateDevicePolicyConfigurationResponse"
    "fixture/UpdateDevicePolicyConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevicePolicyConfiguration)

responseUpdateDomainMetadata :: UpdateDomainMetadataResponse -> TestTree
responseUpdateDomainMetadata =
  res
    "UpdateDomainMetadataResponse"
    "fixture/UpdateDomainMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainMetadata)

responseUpdateFleetMetadata :: UpdateFleetMetadataResponse -> TestTree
responseUpdateFleetMetadata =
  res
    "UpdateFleetMetadataResponse"
    "fixture/UpdateFleetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetMetadata)

responseUpdateIdentityProviderConfiguration :: UpdateIdentityProviderConfigurationResponse -> TestTree
responseUpdateIdentityProviderConfiguration =
  res
    "UpdateIdentityProviderConfigurationResponse"
    "fixture/UpdateIdentityProviderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProviderConfiguration)
