{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SupportApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SupportApp where

import Amazonka.SupportApp
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SupportApp.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSlackChannelConfiguration $
--             newCreateSlackChannelConfiguration
--
--         , requestDeleteAccountAlias $
--             newDeleteAccountAlias
--
--         , requestDeleteSlackChannelConfiguration $
--             newDeleteSlackChannelConfiguration
--
--         , requestDeleteSlackWorkspaceConfiguration $
--             newDeleteSlackWorkspaceConfiguration
--
--         , requestGetAccountAlias $
--             newGetAccountAlias
--
--         , requestListSlackChannelConfigurations $
--             newListSlackChannelConfigurations
--
--         , requestListSlackWorkspaceConfigurations $
--             newListSlackWorkspaceConfigurations
--
--         , requestPutAccountAlias $
--             newPutAccountAlias
--
--         , requestRegisterSlackWorkspaceForOrganization $
--             newRegisterSlackWorkspaceForOrganization
--
--         , requestUpdateSlackChannelConfiguration $
--             newUpdateSlackChannelConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateSlackChannelConfiguration $
--             newCreateSlackChannelConfigurationResponse
--
--         , responseDeleteAccountAlias $
--             newDeleteAccountAliasResponse
--
--         , responseDeleteSlackChannelConfiguration $
--             newDeleteSlackChannelConfigurationResponse
--
--         , responseDeleteSlackWorkspaceConfiguration $
--             newDeleteSlackWorkspaceConfigurationResponse
--
--         , responseGetAccountAlias $
--             newGetAccountAliasResponse
--
--         , responseListSlackChannelConfigurations $
--             newListSlackChannelConfigurationsResponse
--
--         , responseListSlackWorkspaceConfigurations $
--             newListSlackWorkspaceConfigurationsResponse
--
--         , responsePutAccountAlias $
--             newPutAccountAliasResponse
--
--         , responseRegisterSlackWorkspaceForOrganization $
--             newRegisterSlackWorkspaceForOrganizationResponse
--
--         , responseUpdateSlackChannelConfiguration $
--             newUpdateSlackChannelConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCreateSlackChannelConfiguration :: CreateSlackChannelConfiguration -> TestTree
requestCreateSlackChannelConfiguration =
  req
    "CreateSlackChannelConfiguration"
    "fixture/CreateSlackChannelConfiguration.yaml"

requestDeleteAccountAlias :: DeleteAccountAlias -> TestTree
requestDeleteAccountAlias =
  req
    "DeleteAccountAlias"
    "fixture/DeleteAccountAlias.yaml"

requestDeleteSlackChannelConfiguration :: DeleteSlackChannelConfiguration -> TestTree
requestDeleteSlackChannelConfiguration =
  req
    "DeleteSlackChannelConfiguration"
    "fixture/DeleteSlackChannelConfiguration.yaml"

requestDeleteSlackWorkspaceConfiguration :: DeleteSlackWorkspaceConfiguration -> TestTree
requestDeleteSlackWorkspaceConfiguration =
  req
    "DeleteSlackWorkspaceConfiguration"
    "fixture/DeleteSlackWorkspaceConfiguration.yaml"

requestGetAccountAlias :: GetAccountAlias -> TestTree
requestGetAccountAlias =
  req
    "GetAccountAlias"
    "fixture/GetAccountAlias.yaml"

requestListSlackChannelConfigurations :: ListSlackChannelConfigurations -> TestTree
requestListSlackChannelConfigurations =
  req
    "ListSlackChannelConfigurations"
    "fixture/ListSlackChannelConfigurations.yaml"

requestListSlackWorkspaceConfigurations :: ListSlackWorkspaceConfigurations -> TestTree
requestListSlackWorkspaceConfigurations =
  req
    "ListSlackWorkspaceConfigurations"
    "fixture/ListSlackWorkspaceConfigurations.yaml"

requestPutAccountAlias :: PutAccountAlias -> TestTree
requestPutAccountAlias =
  req
    "PutAccountAlias"
    "fixture/PutAccountAlias.yaml"

requestRegisterSlackWorkspaceForOrganization :: RegisterSlackWorkspaceForOrganization -> TestTree
requestRegisterSlackWorkspaceForOrganization =
  req
    "RegisterSlackWorkspaceForOrganization"
    "fixture/RegisterSlackWorkspaceForOrganization.yaml"

requestUpdateSlackChannelConfiguration :: UpdateSlackChannelConfiguration -> TestTree
requestUpdateSlackChannelConfiguration =
  req
    "UpdateSlackChannelConfiguration"
    "fixture/UpdateSlackChannelConfiguration.yaml"

-- Responses

responseCreateSlackChannelConfiguration :: CreateSlackChannelConfigurationResponse -> TestTree
responseCreateSlackChannelConfiguration =
  res
    "CreateSlackChannelConfigurationResponse"
    "fixture/CreateSlackChannelConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSlackChannelConfiguration)

responseDeleteAccountAlias :: DeleteAccountAliasResponse -> TestTree
responseDeleteAccountAlias =
  res
    "DeleteAccountAliasResponse"
    "fixture/DeleteAccountAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAlias)

responseDeleteSlackChannelConfiguration :: DeleteSlackChannelConfigurationResponse -> TestTree
responseDeleteSlackChannelConfiguration =
  res
    "DeleteSlackChannelConfigurationResponse"
    "fixture/DeleteSlackChannelConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlackChannelConfiguration)

responseDeleteSlackWorkspaceConfiguration :: DeleteSlackWorkspaceConfigurationResponse -> TestTree
responseDeleteSlackWorkspaceConfiguration =
  res
    "DeleteSlackWorkspaceConfigurationResponse"
    "fixture/DeleteSlackWorkspaceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlackWorkspaceConfiguration)

responseGetAccountAlias :: GetAccountAliasResponse -> TestTree
responseGetAccountAlias =
  res
    "GetAccountAliasResponse"
    "fixture/GetAccountAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountAlias)

responseListSlackChannelConfigurations :: ListSlackChannelConfigurationsResponse -> TestTree
responseListSlackChannelConfigurations =
  res
    "ListSlackChannelConfigurationsResponse"
    "fixture/ListSlackChannelConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSlackChannelConfigurations)

responseListSlackWorkspaceConfigurations :: ListSlackWorkspaceConfigurationsResponse -> TestTree
responseListSlackWorkspaceConfigurations =
  res
    "ListSlackWorkspaceConfigurationsResponse"
    "fixture/ListSlackWorkspaceConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSlackWorkspaceConfigurations)

responsePutAccountAlias :: PutAccountAliasResponse -> TestTree
responsePutAccountAlias =
  res
    "PutAccountAliasResponse"
    "fixture/PutAccountAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountAlias)

responseRegisterSlackWorkspaceForOrganization :: RegisterSlackWorkspaceForOrganizationResponse -> TestTree
responseRegisterSlackWorkspaceForOrganization =
  res
    "RegisterSlackWorkspaceForOrganizationResponse"
    "fixture/RegisterSlackWorkspaceForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterSlackWorkspaceForOrganization)

responseUpdateSlackChannelConfiguration :: UpdateSlackChannelConfigurationResponse -> TestTree
responseUpdateSlackChannelConfiguration =
  res
    "UpdateSlackChannelConfigurationResponse"
    "fixture/UpdateSlackChannelConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSlackChannelConfiguration)
