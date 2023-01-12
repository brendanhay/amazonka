{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppMesh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppMesh where

import Amazonka.AppMesh
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppMesh.Internal
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
--         [ requestCreateGatewayRoute $
--             newCreateGatewayRoute
--
--         , requestCreateMesh $
--             newCreateMesh
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateVirtualGateway $
--             newCreateVirtualGateway
--
--         , requestCreateVirtualNode $
--             newCreateVirtualNode
--
--         , requestCreateVirtualRouter $
--             newCreateVirtualRouter
--
--         , requestCreateVirtualService $
--             newCreateVirtualService
--
--         , requestDeleteGatewayRoute $
--             newDeleteGatewayRoute
--
--         , requestDeleteMesh $
--             newDeleteMesh
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDeleteVirtualGateway $
--             newDeleteVirtualGateway
--
--         , requestDeleteVirtualNode $
--             newDeleteVirtualNode
--
--         , requestDeleteVirtualRouter $
--             newDeleteVirtualRouter
--
--         , requestDeleteVirtualService $
--             newDeleteVirtualService
--
--         , requestDescribeGatewayRoute $
--             newDescribeGatewayRoute
--
--         , requestDescribeMesh $
--             newDescribeMesh
--
--         , requestDescribeRoute $
--             newDescribeRoute
--
--         , requestDescribeVirtualGateway $
--             newDescribeVirtualGateway
--
--         , requestDescribeVirtualNode $
--             newDescribeVirtualNode
--
--         , requestDescribeVirtualRouter $
--             newDescribeVirtualRouter
--
--         , requestDescribeVirtualService $
--             newDescribeVirtualService
--
--         , requestListGatewayRoutes $
--             newListGatewayRoutes
--
--         , requestListMeshes $
--             newListMeshes
--
--         , requestListRoutes $
--             newListRoutes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVirtualGateways $
--             newListVirtualGateways
--
--         , requestListVirtualNodes $
--             newListVirtualNodes
--
--         , requestListVirtualRouters $
--             newListVirtualRouters
--
--         , requestListVirtualServices $
--             newListVirtualServices
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateGatewayRoute $
--             newUpdateGatewayRoute
--
--         , requestUpdateMesh $
--             newUpdateMesh
--
--         , requestUpdateRoute $
--             newUpdateRoute
--
--         , requestUpdateVirtualGateway $
--             newUpdateVirtualGateway
--
--         , requestUpdateVirtualNode $
--             newUpdateVirtualNode
--
--         , requestUpdateVirtualRouter $
--             newUpdateVirtualRouter
--
--         , requestUpdateVirtualService $
--             newUpdateVirtualService
--
--           ]

--     , testGroup "response"
--         [ responseCreateGatewayRoute $
--             newCreateGatewayRouteResponse
--
--         , responseCreateMesh $
--             newCreateMeshResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseCreateVirtualGateway $
--             newCreateVirtualGatewayResponse
--
--         , responseCreateVirtualNode $
--             newCreateVirtualNodeResponse
--
--         , responseCreateVirtualRouter $
--             newCreateVirtualRouterResponse
--
--         , responseCreateVirtualService $
--             newCreateVirtualServiceResponse
--
--         , responseDeleteGatewayRoute $
--             newDeleteGatewayRouteResponse
--
--         , responseDeleteMesh $
--             newDeleteMeshResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseDeleteVirtualGateway $
--             newDeleteVirtualGatewayResponse
--
--         , responseDeleteVirtualNode $
--             newDeleteVirtualNodeResponse
--
--         , responseDeleteVirtualRouter $
--             newDeleteVirtualRouterResponse
--
--         , responseDeleteVirtualService $
--             newDeleteVirtualServiceResponse
--
--         , responseDescribeGatewayRoute $
--             newDescribeGatewayRouteResponse
--
--         , responseDescribeMesh $
--             newDescribeMeshResponse
--
--         , responseDescribeRoute $
--             newDescribeRouteResponse
--
--         , responseDescribeVirtualGateway $
--             newDescribeVirtualGatewayResponse
--
--         , responseDescribeVirtualNode $
--             newDescribeVirtualNodeResponse
--
--         , responseDescribeVirtualRouter $
--             newDescribeVirtualRouterResponse
--
--         , responseDescribeVirtualService $
--             newDescribeVirtualServiceResponse
--
--         , responseListGatewayRoutes $
--             newListGatewayRoutesResponse
--
--         , responseListMeshes $
--             newListMeshesResponse
--
--         , responseListRoutes $
--             newListRoutesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVirtualGateways $
--             newListVirtualGatewaysResponse
--
--         , responseListVirtualNodes $
--             newListVirtualNodesResponse
--
--         , responseListVirtualRouters $
--             newListVirtualRoutersResponse
--
--         , responseListVirtualServices $
--             newListVirtualServicesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateGatewayRoute $
--             newUpdateGatewayRouteResponse
--
--         , responseUpdateMesh $
--             newUpdateMeshResponse
--
--         , responseUpdateRoute $
--             newUpdateRouteResponse
--
--         , responseUpdateVirtualGateway $
--             newUpdateVirtualGatewayResponse
--
--         , responseUpdateVirtualNode $
--             newUpdateVirtualNodeResponse
--
--         , responseUpdateVirtualRouter $
--             newUpdateVirtualRouterResponse
--
--         , responseUpdateVirtualService $
--             newUpdateVirtualServiceResponse
--
--           ]
--     ]

-- Requests

requestCreateGatewayRoute :: CreateGatewayRoute -> TestTree
requestCreateGatewayRoute =
  req
    "CreateGatewayRoute"
    "fixture/CreateGatewayRoute.yaml"

requestCreateMesh :: CreateMesh -> TestTree
requestCreateMesh =
  req
    "CreateMesh"
    "fixture/CreateMesh.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateVirtualGateway :: CreateVirtualGateway -> TestTree
requestCreateVirtualGateway =
  req
    "CreateVirtualGateway"
    "fixture/CreateVirtualGateway.yaml"

requestCreateVirtualNode :: CreateVirtualNode -> TestTree
requestCreateVirtualNode =
  req
    "CreateVirtualNode"
    "fixture/CreateVirtualNode.yaml"

requestCreateVirtualRouter :: CreateVirtualRouter -> TestTree
requestCreateVirtualRouter =
  req
    "CreateVirtualRouter"
    "fixture/CreateVirtualRouter.yaml"

requestCreateVirtualService :: CreateVirtualService -> TestTree
requestCreateVirtualService =
  req
    "CreateVirtualService"
    "fixture/CreateVirtualService.yaml"

requestDeleteGatewayRoute :: DeleteGatewayRoute -> TestTree
requestDeleteGatewayRoute =
  req
    "DeleteGatewayRoute"
    "fixture/DeleteGatewayRoute.yaml"

requestDeleteMesh :: DeleteMesh -> TestTree
requestDeleteMesh =
  req
    "DeleteMesh"
    "fixture/DeleteMesh.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestDeleteVirtualGateway :: DeleteVirtualGateway -> TestTree
requestDeleteVirtualGateway =
  req
    "DeleteVirtualGateway"
    "fixture/DeleteVirtualGateway.yaml"

requestDeleteVirtualNode :: DeleteVirtualNode -> TestTree
requestDeleteVirtualNode =
  req
    "DeleteVirtualNode"
    "fixture/DeleteVirtualNode.yaml"

requestDeleteVirtualRouter :: DeleteVirtualRouter -> TestTree
requestDeleteVirtualRouter =
  req
    "DeleteVirtualRouter"
    "fixture/DeleteVirtualRouter.yaml"

requestDeleteVirtualService :: DeleteVirtualService -> TestTree
requestDeleteVirtualService =
  req
    "DeleteVirtualService"
    "fixture/DeleteVirtualService.yaml"

requestDescribeGatewayRoute :: DescribeGatewayRoute -> TestTree
requestDescribeGatewayRoute =
  req
    "DescribeGatewayRoute"
    "fixture/DescribeGatewayRoute.yaml"

requestDescribeMesh :: DescribeMesh -> TestTree
requestDescribeMesh =
  req
    "DescribeMesh"
    "fixture/DescribeMesh.yaml"

requestDescribeRoute :: DescribeRoute -> TestTree
requestDescribeRoute =
  req
    "DescribeRoute"
    "fixture/DescribeRoute.yaml"

requestDescribeVirtualGateway :: DescribeVirtualGateway -> TestTree
requestDescribeVirtualGateway =
  req
    "DescribeVirtualGateway"
    "fixture/DescribeVirtualGateway.yaml"

requestDescribeVirtualNode :: DescribeVirtualNode -> TestTree
requestDescribeVirtualNode =
  req
    "DescribeVirtualNode"
    "fixture/DescribeVirtualNode.yaml"

requestDescribeVirtualRouter :: DescribeVirtualRouter -> TestTree
requestDescribeVirtualRouter =
  req
    "DescribeVirtualRouter"
    "fixture/DescribeVirtualRouter.yaml"

requestDescribeVirtualService :: DescribeVirtualService -> TestTree
requestDescribeVirtualService =
  req
    "DescribeVirtualService"
    "fixture/DescribeVirtualService.yaml"

requestListGatewayRoutes :: ListGatewayRoutes -> TestTree
requestListGatewayRoutes =
  req
    "ListGatewayRoutes"
    "fixture/ListGatewayRoutes.yaml"

requestListMeshes :: ListMeshes -> TestTree
requestListMeshes =
  req
    "ListMeshes"
    "fixture/ListMeshes.yaml"

requestListRoutes :: ListRoutes -> TestTree
requestListRoutes =
  req
    "ListRoutes"
    "fixture/ListRoutes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVirtualGateways :: ListVirtualGateways -> TestTree
requestListVirtualGateways =
  req
    "ListVirtualGateways"
    "fixture/ListVirtualGateways.yaml"

requestListVirtualNodes :: ListVirtualNodes -> TestTree
requestListVirtualNodes =
  req
    "ListVirtualNodes"
    "fixture/ListVirtualNodes.yaml"

requestListVirtualRouters :: ListVirtualRouters -> TestTree
requestListVirtualRouters =
  req
    "ListVirtualRouters"
    "fixture/ListVirtualRouters.yaml"

requestListVirtualServices :: ListVirtualServices -> TestTree
requestListVirtualServices =
  req
    "ListVirtualServices"
    "fixture/ListVirtualServices.yaml"

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

requestUpdateGatewayRoute :: UpdateGatewayRoute -> TestTree
requestUpdateGatewayRoute =
  req
    "UpdateGatewayRoute"
    "fixture/UpdateGatewayRoute.yaml"

requestUpdateMesh :: UpdateMesh -> TestTree
requestUpdateMesh =
  req
    "UpdateMesh"
    "fixture/UpdateMesh.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute =
  req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestUpdateVirtualGateway :: UpdateVirtualGateway -> TestTree
requestUpdateVirtualGateway =
  req
    "UpdateVirtualGateway"
    "fixture/UpdateVirtualGateway.yaml"

requestUpdateVirtualNode :: UpdateVirtualNode -> TestTree
requestUpdateVirtualNode =
  req
    "UpdateVirtualNode"
    "fixture/UpdateVirtualNode.yaml"

requestUpdateVirtualRouter :: UpdateVirtualRouter -> TestTree
requestUpdateVirtualRouter =
  req
    "UpdateVirtualRouter"
    "fixture/UpdateVirtualRouter.yaml"

requestUpdateVirtualService :: UpdateVirtualService -> TestTree
requestUpdateVirtualService =
  req
    "UpdateVirtualService"
    "fixture/UpdateVirtualService.yaml"

-- Responses

responseCreateGatewayRoute :: CreateGatewayRouteResponse -> TestTree
responseCreateGatewayRoute =
  res
    "CreateGatewayRouteResponse"
    "fixture/CreateGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGatewayRoute)

responseCreateMesh :: CreateMeshResponse -> TestTree
responseCreateMesh =
  res
    "CreateMeshResponse"
    "fixture/CreateMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMesh)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateVirtualGateway :: CreateVirtualGatewayResponse -> TestTree
responseCreateVirtualGateway =
  res
    "CreateVirtualGatewayResponse"
    "fixture/CreateVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualGateway)

responseCreateVirtualNode :: CreateVirtualNodeResponse -> TestTree
responseCreateVirtualNode =
  res
    "CreateVirtualNodeResponse"
    "fixture/CreateVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualNode)

responseCreateVirtualRouter :: CreateVirtualRouterResponse -> TestTree
responseCreateVirtualRouter =
  res
    "CreateVirtualRouterResponse"
    "fixture/CreateVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualRouter)

responseCreateVirtualService :: CreateVirtualServiceResponse -> TestTree
responseCreateVirtualService =
  res
    "CreateVirtualServiceResponse"
    "fixture/CreateVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualService)

responseDeleteGatewayRoute :: DeleteGatewayRouteResponse -> TestTree
responseDeleteGatewayRoute =
  res
    "DeleteGatewayRouteResponse"
    "fixture/DeleteGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGatewayRoute)

responseDeleteMesh :: DeleteMeshResponse -> TestTree
responseDeleteMesh =
  res
    "DeleteMeshResponse"
    "fixture/DeleteMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMesh)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseDeleteVirtualGateway :: DeleteVirtualGatewayResponse -> TestTree
responseDeleteVirtualGateway =
  res
    "DeleteVirtualGatewayResponse"
    "fixture/DeleteVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualGateway)

responseDeleteVirtualNode :: DeleteVirtualNodeResponse -> TestTree
responseDeleteVirtualNode =
  res
    "DeleteVirtualNodeResponse"
    "fixture/DeleteVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualNode)

responseDeleteVirtualRouter :: DeleteVirtualRouterResponse -> TestTree
responseDeleteVirtualRouter =
  res
    "DeleteVirtualRouterResponse"
    "fixture/DeleteVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualRouter)

responseDeleteVirtualService :: DeleteVirtualServiceResponse -> TestTree
responseDeleteVirtualService =
  res
    "DeleteVirtualServiceResponse"
    "fixture/DeleteVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualService)

responseDescribeGatewayRoute :: DescribeGatewayRouteResponse -> TestTree
responseDescribeGatewayRoute =
  res
    "DescribeGatewayRouteResponse"
    "fixture/DescribeGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayRoute)

responseDescribeMesh :: DescribeMeshResponse -> TestTree
responseDescribeMesh =
  res
    "DescribeMeshResponse"
    "fixture/DescribeMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMesh)

responseDescribeRoute :: DescribeRouteResponse -> TestTree
responseDescribeRoute =
  res
    "DescribeRouteResponse"
    "fixture/DescribeRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoute)

responseDescribeVirtualGateway :: DescribeVirtualGatewayResponse -> TestTree
responseDescribeVirtualGateway =
  res
    "DescribeVirtualGatewayResponse"
    "fixture/DescribeVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualGateway)

responseDescribeVirtualNode :: DescribeVirtualNodeResponse -> TestTree
responseDescribeVirtualNode =
  res
    "DescribeVirtualNodeResponse"
    "fixture/DescribeVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualNode)

responseDescribeVirtualRouter :: DescribeVirtualRouterResponse -> TestTree
responseDescribeVirtualRouter =
  res
    "DescribeVirtualRouterResponse"
    "fixture/DescribeVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualRouter)

responseDescribeVirtualService :: DescribeVirtualServiceResponse -> TestTree
responseDescribeVirtualService =
  res
    "DescribeVirtualServiceResponse"
    "fixture/DescribeVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualService)

responseListGatewayRoutes :: ListGatewayRoutesResponse -> TestTree
responseListGatewayRoutes =
  res
    "ListGatewayRoutesResponse"
    "fixture/ListGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGatewayRoutes)

responseListMeshes :: ListMeshesResponse -> TestTree
responseListMeshes =
  res
    "ListMeshesResponse"
    "fixture/ListMeshesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMeshes)

responseListRoutes :: ListRoutesResponse -> TestTree
responseListRoutes =
  res
    "ListRoutesResponse"
    "fixture/ListRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVirtualGateways :: ListVirtualGatewaysResponse -> TestTree
responseListVirtualGateways =
  res
    "ListVirtualGatewaysResponse"
    "fixture/ListVirtualGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualGateways)

responseListVirtualNodes :: ListVirtualNodesResponse -> TestTree
responseListVirtualNodes =
  res
    "ListVirtualNodesResponse"
    "fixture/ListVirtualNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualNodes)

responseListVirtualRouters :: ListVirtualRoutersResponse -> TestTree
responseListVirtualRouters =
  res
    "ListVirtualRoutersResponse"
    "fixture/ListVirtualRoutersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualRouters)

responseListVirtualServices :: ListVirtualServicesResponse -> TestTree
responseListVirtualServices =
  res
    "ListVirtualServicesResponse"
    "fixture/ListVirtualServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualServices)

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

responseUpdateGatewayRoute :: UpdateGatewayRouteResponse -> TestTree
responseUpdateGatewayRoute =
  res
    "UpdateGatewayRouteResponse"
    "fixture/UpdateGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayRoute)

responseUpdateMesh :: UpdateMeshResponse -> TestTree
responseUpdateMesh =
  res
    "UpdateMeshResponse"
    "fixture/UpdateMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMesh)

responseUpdateRoute :: UpdateRouteResponse -> TestTree
responseUpdateRoute =
  res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoute)

responseUpdateVirtualGateway :: UpdateVirtualGatewayResponse -> TestTree
responseUpdateVirtualGateway =
  res
    "UpdateVirtualGatewayResponse"
    "fixture/UpdateVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualGateway)

responseUpdateVirtualNode :: UpdateVirtualNodeResponse -> TestTree
responseUpdateVirtualNode =
  res
    "UpdateVirtualNodeResponse"
    "fixture/UpdateVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualNode)

responseUpdateVirtualRouter :: UpdateVirtualRouterResponse -> TestTree
responseUpdateVirtualRouter =
  res
    "UpdateVirtualRouterResponse"
    "fixture/UpdateVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualRouter)

responseUpdateVirtualService :: UpdateVirtualServiceResponse -> TestTree
responseUpdateVirtualService =
  res
    "UpdateVirtualServiceResponse"
    "fixture/UpdateVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualService)
