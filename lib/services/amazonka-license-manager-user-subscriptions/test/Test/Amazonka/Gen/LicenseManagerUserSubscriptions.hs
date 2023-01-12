{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LicenseManagerUserSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LicenseManagerUserSubscriptions where

import Amazonka.LicenseManagerUserSubscriptions
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LicenseManagerUserSubscriptions.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateUser $
--             newAssociateUser
--
--         , requestDeregisterIdentityProvider $
--             newDeregisterIdentityProvider
--
--         , requestDisassociateUser $
--             newDisassociateUser
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListProductSubscriptions $
--             newListProductSubscriptions
--
--         , requestListUserAssociations $
--             newListUserAssociations
--
--         , requestRegisterIdentityProvider $
--             newRegisterIdentityProvider
--
--         , requestStartProductSubscription $
--             newStartProductSubscription
--
--         , requestStopProductSubscription $
--             newStopProductSubscription
--
--         , requestUpdateIdentityProviderSettings $
--             newUpdateIdentityProviderSettings
--
--           ]

--     , testGroup "response"
--         [ responseAssociateUser $
--             newAssociateUserResponse
--
--         , responseDeregisterIdentityProvider $
--             newDeregisterIdentityProviderResponse
--
--         , responseDisassociateUser $
--             newDisassociateUserResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListProductSubscriptions $
--             newListProductSubscriptionsResponse
--
--         , responseListUserAssociations $
--             newListUserAssociationsResponse
--
--         , responseRegisterIdentityProvider $
--             newRegisterIdentityProviderResponse
--
--         , responseStartProductSubscription $
--             newStartProductSubscriptionResponse
--
--         , responseStopProductSubscription $
--             newStopProductSubscriptionResponse
--
--         , responseUpdateIdentityProviderSettings $
--             newUpdateIdentityProviderSettingsResponse
--
--           ]
--     ]

-- Requests

requestAssociateUser :: AssociateUser -> TestTree
requestAssociateUser =
  req
    "AssociateUser"
    "fixture/AssociateUser.yaml"

requestDeregisterIdentityProvider :: DeregisterIdentityProvider -> TestTree
requestDeregisterIdentityProvider =
  req
    "DeregisterIdentityProvider"
    "fixture/DeregisterIdentityProvider.yaml"

requestDisassociateUser :: DisassociateUser -> TestTree
requestDisassociateUser =
  req
    "DisassociateUser"
    "fixture/DisassociateUser.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders =
  req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListProductSubscriptions :: ListProductSubscriptions -> TestTree
requestListProductSubscriptions =
  req
    "ListProductSubscriptions"
    "fixture/ListProductSubscriptions.yaml"

requestListUserAssociations :: ListUserAssociations -> TestTree
requestListUserAssociations =
  req
    "ListUserAssociations"
    "fixture/ListUserAssociations.yaml"

requestRegisterIdentityProvider :: RegisterIdentityProvider -> TestTree
requestRegisterIdentityProvider =
  req
    "RegisterIdentityProvider"
    "fixture/RegisterIdentityProvider.yaml"

requestStartProductSubscription :: StartProductSubscription -> TestTree
requestStartProductSubscription =
  req
    "StartProductSubscription"
    "fixture/StartProductSubscription.yaml"

requestStopProductSubscription :: StopProductSubscription -> TestTree
requestStopProductSubscription =
  req
    "StopProductSubscription"
    "fixture/StopProductSubscription.yaml"

requestUpdateIdentityProviderSettings :: UpdateIdentityProviderSettings -> TestTree
requestUpdateIdentityProviderSettings =
  req
    "UpdateIdentityProviderSettings"
    "fixture/UpdateIdentityProviderSettings.yaml"

-- Responses

responseAssociateUser :: AssociateUserResponse -> TestTree
responseAssociateUser =
  res
    "AssociateUserResponse"
    "fixture/AssociateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateUser)

responseDeregisterIdentityProvider :: DeregisterIdentityProviderResponse -> TestTree
responseDeregisterIdentityProvider =
  res
    "DeregisterIdentityProviderResponse"
    "fixture/DeregisterIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterIdentityProvider)

responseDisassociateUser :: DisassociateUserResponse -> TestTree
responseDisassociateUser =
  res
    "DisassociateUserResponse"
    "fixture/DisassociateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateUser)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityProviders)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseListProductSubscriptions :: ListProductSubscriptionsResponse -> TestTree
responseListProductSubscriptions =
  res
    "ListProductSubscriptionsResponse"
    "fixture/ListProductSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProductSubscriptions)

responseListUserAssociations :: ListUserAssociationsResponse -> TestTree
responseListUserAssociations =
  res
    "ListUserAssociationsResponse"
    "fixture/ListUserAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserAssociations)

responseRegisterIdentityProvider :: RegisterIdentityProviderResponse -> TestTree
responseRegisterIdentityProvider =
  res
    "RegisterIdentityProviderResponse"
    "fixture/RegisterIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterIdentityProvider)

responseStartProductSubscription :: StartProductSubscriptionResponse -> TestTree
responseStartProductSubscription =
  res
    "StartProductSubscriptionResponse"
    "fixture/StartProductSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProductSubscription)

responseStopProductSubscription :: StopProductSubscriptionResponse -> TestTree
responseStopProductSubscription =
  res
    "StopProductSubscriptionResponse"
    "fixture/StopProductSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopProductSubscription)

responseUpdateIdentityProviderSettings :: UpdateIdentityProviderSettingsResponse -> TestTree
responseUpdateIdentityProviderSettings =
  res
    "UpdateIdentityProviderSettingsResponse"
    "fixture/UpdateIdentityProviderSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProviderSettings)
