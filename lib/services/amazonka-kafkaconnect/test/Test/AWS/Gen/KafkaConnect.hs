{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KafkaConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KafkaConnect where

import Amazonka.KafkaConnect
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.KafkaConnect.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListWorkerConfigurations $
--             newListWorkerConfigurations
--
--         , requestDescribeCustomPlugin $
--             newDescribeCustomPlugin
--
--         , requestDeleteConnector $
--             newDeleteConnector
--
--         , requestUpdateConnector $
--             newUpdateConnector
--
--         , requestCreateWorkerConfiguration $
--             newCreateWorkerConfiguration
--
--         , requestListConnectors $
--             newListConnectors
--
--         , requestListCustomPlugins $
--             newListCustomPlugins
--
--         , requestCreateConnector $
--             newCreateConnector
--
--         , requestDescribeWorkerConfiguration $
--             newDescribeWorkerConfiguration
--
--         , requestDescribeConnector $
--             newDescribeConnector
--
--         , requestCreateCustomPlugin $
--             newCreateCustomPlugin
--
--           ]

--     , testGroup "response"
--         [ responseListWorkerConfigurations $
--             newListWorkerConfigurationsResponse
--
--         , responseDescribeCustomPlugin $
--             newDescribeCustomPluginResponse
--
--         , responseDeleteConnector $
--             newDeleteConnectorResponse
--
--         , responseUpdateConnector $
--             newUpdateConnectorResponse
--
--         , responseCreateWorkerConfiguration $
--             newCreateWorkerConfigurationResponse
--
--         , responseListConnectors $
--             newListConnectorsResponse
--
--         , responseListCustomPlugins $
--             newListCustomPluginsResponse
--
--         , responseCreateConnector $
--             newCreateConnectorResponse
--
--         , responseDescribeWorkerConfiguration $
--             newDescribeWorkerConfigurationResponse
--
--         , responseDescribeConnector $
--             newDescribeConnectorResponse
--
--         , responseCreateCustomPlugin $
--             newCreateCustomPluginResponse
--
--           ]
--     ]

-- Requests

requestListWorkerConfigurations :: ListWorkerConfigurations -> TestTree
requestListWorkerConfigurations =
  req
    "ListWorkerConfigurations"
    "fixture/ListWorkerConfigurations.yaml"

requestDescribeCustomPlugin :: DescribeCustomPlugin -> TestTree
requestDescribeCustomPlugin =
  req
    "DescribeCustomPlugin"
    "fixture/DescribeCustomPlugin.yaml"

requestDeleteConnector :: DeleteConnector -> TestTree
requestDeleteConnector =
  req
    "DeleteConnector"
    "fixture/DeleteConnector.yaml"

requestUpdateConnector :: UpdateConnector -> TestTree
requestUpdateConnector =
  req
    "UpdateConnector"
    "fixture/UpdateConnector.yaml"

requestCreateWorkerConfiguration :: CreateWorkerConfiguration -> TestTree
requestCreateWorkerConfiguration =
  req
    "CreateWorkerConfiguration"
    "fixture/CreateWorkerConfiguration.yaml"

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

requestCreateConnector :: CreateConnector -> TestTree
requestCreateConnector =
  req
    "CreateConnector"
    "fixture/CreateConnector.yaml"

requestDescribeWorkerConfiguration :: DescribeWorkerConfiguration -> TestTree
requestDescribeWorkerConfiguration =
  req
    "DescribeWorkerConfiguration"
    "fixture/DescribeWorkerConfiguration.yaml"

requestDescribeConnector :: DescribeConnector -> TestTree
requestDescribeConnector =
  req
    "DescribeConnector"
    "fixture/DescribeConnector.yaml"

requestCreateCustomPlugin :: CreateCustomPlugin -> TestTree
requestCreateCustomPlugin =
  req
    "CreateCustomPlugin"
    "fixture/CreateCustomPlugin.yaml"

-- Responses

responseListWorkerConfigurations :: ListWorkerConfigurationsResponse -> TestTree
responseListWorkerConfigurations =
  res
    "ListWorkerConfigurationsResponse"
    "fixture/ListWorkerConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkerConfigurations)

responseDescribeCustomPlugin :: DescribeCustomPluginResponse -> TestTree
responseDescribeCustomPlugin =
  res
    "DescribeCustomPluginResponse"
    "fixture/DescribeCustomPluginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomPlugin)

responseDeleteConnector :: DeleteConnectorResponse -> TestTree
responseDeleteConnector =
  res
    "DeleteConnectorResponse"
    "fixture/DeleteConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnector)

responseUpdateConnector :: UpdateConnectorResponse -> TestTree
responseUpdateConnector =
  res
    "UpdateConnectorResponse"
    "fixture/UpdateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnector)

responseCreateWorkerConfiguration :: CreateWorkerConfigurationResponse -> TestTree
responseCreateWorkerConfiguration =
  res
    "CreateWorkerConfigurationResponse"
    "fixture/CreateWorkerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkerConfiguration)

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

responseCreateConnector :: CreateConnectorResponse -> TestTree
responseCreateConnector =
  res
    "CreateConnectorResponse"
    "fixture/CreateConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnector)

responseDescribeWorkerConfiguration :: DescribeWorkerConfigurationResponse -> TestTree
responseDescribeWorkerConfiguration =
  res
    "DescribeWorkerConfigurationResponse"
    "fixture/DescribeWorkerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkerConfiguration)

responseDescribeConnector :: DescribeConnectorResponse -> TestTree
responseDescribeConnector =
  res
    "DescribeConnectorResponse"
    "fixture/DescribeConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnector)

responseCreateCustomPlugin :: CreateCustomPluginResponse -> TestTree
responseCreateCustomPlugin =
  res
    "CreateCustomPluginResponse"
    "fixture/CreateCustomPluginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomPlugin)
