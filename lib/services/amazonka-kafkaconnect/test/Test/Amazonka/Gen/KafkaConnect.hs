{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KafkaConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KafkaConnect where

import Amazonka.KafkaConnect
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KafkaConnect.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateConnector $
--             newCreateConnector
--
--         , requestCreateCustomPlugin $
--             newCreateCustomPlugin
--
--         , requestCreateWorkerConfiguration $
--             newCreateWorkerConfiguration
--
--         , requestDeleteConnector $
--             newDeleteConnector
--
--         , requestDeleteCustomPlugin $
--             newDeleteCustomPlugin
--
--         , requestDescribeConnector $
--             newDescribeConnector
--
--         , requestDescribeCustomPlugin $
--             newDescribeCustomPlugin
--
--         , requestDescribeWorkerConfiguration $
--             newDescribeWorkerConfiguration
--
--         , requestListConnectors $
--             newListConnectors
--
--         , requestListCustomPlugins $
--             newListCustomPlugins
--
--         , requestListWorkerConfigurations $
--             newListWorkerConfigurations
--
--         , requestUpdateConnector $
--             newUpdateConnector
--
--           ]

--     , testGroup "response"
--         [ responseCreateConnector $
--             newCreateConnectorResponse
--
--         , responseCreateCustomPlugin $
--             newCreateCustomPluginResponse
--
--         , responseCreateWorkerConfiguration $
--             newCreateWorkerConfigurationResponse
--
--         , responseDeleteConnector $
--             newDeleteConnectorResponse
--
--         , responseDeleteCustomPlugin $
--             newDeleteCustomPluginResponse
--
--         , responseDescribeConnector $
--             newDescribeConnectorResponse
--
--         , responseDescribeCustomPlugin $
--             newDescribeCustomPluginResponse
--
--         , responseDescribeWorkerConfiguration $
--             newDescribeWorkerConfigurationResponse
--
--         , responseListConnectors $
--             newListConnectorsResponse
--
--         , responseListCustomPlugins $
--             newListCustomPluginsResponse
--
--         , responseListWorkerConfigurations $
--             newListWorkerConfigurationsResponse
--
--         , responseUpdateConnector $
--             newUpdateConnectorResponse
--
--           ]
--     ]

-- Requests

requestCreateConnector :: CreateConnector -> TestTree
requestCreateConnector =
  req
    "CreateConnector"
    "fixture/CreateConnector.yaml"

requestCreateCustomPlugin :: CreateCustomPlugin -> TestTree
requestCreateCustomPlugin =
  req
    "CreateCustomPlugin"
    "fixture/CreateCustomPlugin.yaml"

requestCreateWorkerConfiguration :: CreateWorkerConfiguration -> TestTree
requestCreateWorkerConfiguration =
  req
    "CreateWorkerConfiguration"
    "fixture/CreateWorkerConfiguration.yaml"

requestDeleteConnector :: DeleteConnector -> TestTree
requestDeleteConnector =
  req
    "DeleteConnector"
    "fixture/DeleteConnector.yaml"

requestDeleteCustomPlugin :: DeleteCustomPlugin -> TestTree
requestDeleteCustomPlugin =
  req
    "DeleteCustomPlugin"
    "fixture/DeleteCustomPlugin.yaml"

requestDescribeConnector :: DescribeConnector -> TestTree
requestDescribeConnector =
  req
    "DescribeConnector"
    "fixture/DescribeConnector.yaml"

requestDescribeCustomPlugin :: DescribeCustomPlugin -> TestTree
requestDescribeCustomPlugin =
  req
    "DescribeCustomPlugin"
    "fixture/DescribeCustomPlugin.yaml"

requestDescribeWorkerConfiguration :: DescribeWorkerConfiguration -> TestTree
requestDescribeWorkerConfiguration =
  req
    "DescribeWorkerConfiguration"
    "fixture/DescribeWorkerConfiguration.yaml"

requestListConnectors :: ListConnectors -> TestTree
requestListConnectors =
  req
    "ListConnectors"
    "fixture/ListConnectors.yaml"

requestListCustomPlugins :: ListCustomPlugins -> TestTree
requestListCustomPlugins =
  req
    "ListCustomPlugins"
    "fixture/ListCustomPlugins.yaml"

requestListWorkerConfigurations :: ListWorkerConfigurations -> TestTree
requestListWorkerConfigurations =
  req
    "ListWorkerConfigurations"
    "fixture/ListWorkerConfigurations.yaml"

requestUpdateConnector :: UpdateConnector -> TestTree
requestUpdateConnector =
  req
    "UpdateConnector"
    "fixture/UpdateConnector.yaml"

-- Responses

responseCreateConnector :: CreateConnectorResponse -> TestTree
responseCreateConnector =
  res
    "CreateConnectorResponse"
    "fixture/CreateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnector)

responseCreateCustomPlugin :: CreateCustomPluginResponse -> TestTree
responseCreateCustomPlugin =
  res
    "CreateCustomPluginResponse"
    "fixture/CreateCustomPluginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomPlugin)

responseCreateWorkerConfiguration :: CreateWorkerConfigurationResponse -> TestTree
responseCreateWorkerConfiguration =
  res
    "CreateWorkerConfigurationResponse"
    "fixture/CreateWorkerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkerConfiguration)

responseDeleteConnector :: DeleteConnectorResponse -> TestTree
responseDeleteConnector =
  res
    "DeleteConnectorResponse"
    "fixture/DeleteConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnector)

responseDeleteCustomPlugin :: DeleteCustomPluginResponse -> TestTree
responseDeleteCustomPlugin =
  res
    "DeleteCustomPluginResponse"
    "fixture/DeleteCustomPluginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomPlugin)

responseDescribeConnector :: DescribeConnectorResponse -> TestTree
responseDescribeConnector =
  res
    "DescribeConnectorResponse"
    "fixture/DescribeConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnector)

responseDescribeCustomPlugin :: DescribeCustomPluginResponse -> TestTree
responseDescribeCustomPlugin =
  res
    "DescribeCustomPluginResponse"
    "fixture/DescribeCustomPluginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomPlugin)

responseDescribeWorkerConfiguration :: DescribeWorkerConfigurationResponse -> TestTree
responseDescribeWorkerConfiguration =
  res
    "DescribeWorkerConfigurationResponse"
    "fixture/DescribeWorkerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkerConfiguration)

responseListConnectors :: ListConnectorsResponse -> TestTree
responseListConnectors =
  res
    "ListConnectorsResponse"
    "fixture/ListConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectors)

responseListCustomPlugins :: ListCustomPluginsResponse -> TestTree
responseListCustomPlugins =
  res
    "ListCustomPluginsResponse"
    "fixture/ListCustomPluginsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomPlugins)

responseListWorkerConfigurations :: ListWorkerConfigurationsResponse -> TestTree
responseListWorkerConfigurations =
  res
    "ListWorkerConfigurationsResponse"
    "fixture/ListWorkerConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkerConfigurations)

responseUpdateConnector :: UpdateConnectorResponse -> TestTree
responseUpdateConnector =
  res
    "UpdateConnectorResponse"
    "fixture/UpdateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnector)
