{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppMesh
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeVirtualNode $
--             newDescribeVirtualNode
--
--         , requestDescribeVirtualGateway $
--             newDescribeVirtualGateway
--
--         , requestDescribeRoute $
--             newDescribeRoute
--
--         , requestDescribeVirtualRouter $
--             newDescribeVirtualRouter
--
--         , requestListMeshes $
--             newListMeshes
--
--         , requestCreateMesh $
--             newCreateMesh
--
--         , requestUpdateMesh $
--             newUpdateMesh
--
--         , requestDeleteMesh $
--             newDeleteMesh
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateVirtualGateway $
--             newCreateVirtualGateway
--
--         , requestListVirtualServices $
--             newListVirtualServices
--
--         , requestDeleteVirtualService $
--             newDeleteVirtualService
--
--         , requestUpdateVirtualService $
--             newUpdateVirtualService
--
--         , requestUpdateVirtualGateway $
--             newUpdateVirtualGateway
--
--         , requestDeleteVirtualGateway $
--             newDeleteVirtualGateway
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestUpdateRoute $
--             newUpdateRoute
--
--         , requestCreateVirtualService $
--             newCreateVirtualService
--
--         , requestDeleteVirtualNode $
--             newDeleteVirtualNode
--
--         , requestUpdateVirtualNode $
--             newUpdateVirtualNode
--
--         , requestListGatewayRoutes $
--             newListGatewayRoutes
--
--         , requestListRoutes $
--             newListRoutes
--
--         , requestListVirtualNodes $
--             newListVirtualNodes
--
--         , requestDeleteVirtualRouter $
--             newDeleteVirtualRouter
--
--         , requestUpdateVirtualRouter $
--             newUpdateVirtualRouter
--
--         , requestCreateVirtualRouter $
--             newCreateVirtualRouter
--
--         , requestDescribeVirtualService $
--             newDescribeVirtualService
--
--         , requestDescribeGatewayRoute $
--             newDescribeGatewayRoute
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateVirtualNode $
--             newCreateVirtualNode
--
--         , requestCreateGatewayRoute $
--             newCreateGatewayRoute
--
--         , requestUpdateGatewayRoute $
--             newUpdateGatewayRoute
--
--         , requestDeleteGatewayRoute $
--             newDeleteGatewayRoute
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListVirtualGateways $
--             newListVirtualGateways
--
--         , requestListVirtualRouters $
--             newListVirtualRouters
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeMesh $
--             newDescribeMesh
--
--           ]

--     , testGroup "response"
--         [ responseDescribeVirtualNode $
--             newDescribeVirtualNodeResponse
--
--         , responseDescribeVirtualGateway $
--             newDescribeVirtualGatewayResponse
--
--         , responseDescribeRoute $
--             newDescribeRouteResponse
--
--         , responseDescribeVirtualRouter $
--             newDescribeVirtualRouterResponse
--
--         , responseListMeshes $
--             newListMeshesResponse
--
--         , responseCreateMesh $
--             newCreateMeshResponse
--
--         , responseUpdateMesh $
--             newUpdateMeshResponse
--
--         , responseDeleteMesh $
--             newDeleteMeshResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateVirtualGateway $
--             newCreateVirtualGatewayResponse
--
--         , responseListVirtualServices $
--             newListVirtualServicesResponse
--
--         , responseDeleteVirtualService $
--             newDeleteVirtualServiceResponse
--
--         , responseUpdateVirtualService $
--             newUpdateVirtualServiceResponse
--
--         , responseUpdateVirtualGateway $
--             newUpdateVirtualGatewayResponse
--
--         , responseDeleteVirtualGateway $
--             newDeleteVirtualGatewayResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseUpdateRoute $
--             newUpdateRouteResponse
--
--         , responseCreateVirtualService $
--             newCreateVirtualServiceResponse
--
--         , responseDeleteVirtualNode $
--             newDeleteVirtualNodeResponse
--
--         , responseUpdateVirtualNode $
--             newUpdateVirtualNodeResponse
--
--         , responseListGatewayRoutes $
--             newListGatewayRoutesResponse
--
--         , responseListRoutes $
--             newListRoutesResponse
--
--         , responseListVirtualNodes $
--             newListVirtualNodesResponse
--
--         , responseDeleteVirtualRouter $
--             newDeleteVirtualRouterResponse
--
--         , responseUpdateVirtualRouter $
--             newUpdateVirtualRouterResponse
--
--         , responseCreateVirtualRouter $
--             newCreateVirtualRouterResponse
--
--         , responseDescribeVirtualService $
--             newDescribeVirtualServiceResponse
--
--         , responseDescribeGatewayRoute $
--             newDescribeGatewayRouteResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseCreateVirtualNode $
--             newCreateVirtualNodeResponse
--
--         , responseCreateGatewayRoute $
--             newCreateGatewayRouteResponse
--
--         , responseUpdateGatewayRoute $
--             newUpdateGatewayRouteResponse
--
--         , responseDeleteGatewayRoute $
--             newDeleteGatewayRouteResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListVirtualGateways $
--             newListVirtualGatewaysResponse
--
--         , responseListVirtualRouters $
--             newListVirtualRoutersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeMesh $
--             newDescribeMeshResponse
--
--           ]
--     ]

-- Requests

requestDescribeVirtualNode :: DescribeVirtualNode -> TestTree
requestDescribeVirtualNode =
  req
    "DescribeVirtualNode"
    "fixture/DescribeVirtualNode.yaml"

requestDescribeVirtualGateway :: DescribeVirtualGateway -> TestTree
requestDescribeVirtualGateway =
  req
    "DescribeVirtualGateway"
    "fixture/DescribeVirtualGateway.yaml"

requestDescribeRoute :: DescribeRoute -> TestTree
requestDescribeRoute =
  req
    "DescribeRoute"
    "fixture/DescribeRoute.yaml"

requestDescribeVirtualRouter :: DescribeVirtualRouter -> TestTree
requestDescribeVirtualRouter =
  req
    "DescribeVirtualRouter"
    "fixture/DescribeVirtualRouter.yaml"

requestListMeshes :: ListMeshes -> TestTree
requestListMeshes =
  req
    "ListMeshes"
    "fixture/ListMeshes.yaml"

requestCreateMesh :: CreateMesh -> TestTree
requestCreateMesh =
  req
    "CreateMesh"
    "fixture/CreateMesh.yaml"

requestUpdateMesh :: UpdateMesh -> TestTree
requestUpdateMesh =
  req
    "UpdateMesh"
    "fixture/UpdateMesh.yaml"

requestDeleteMesh :: DeleteMesh -> TestTree
requestDeleteMesh =
  req
    "DeleteMesh"
    "fixture/DeleteMesh.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateVirtualGateway :: CreateVirtualGateway -> TestTree
requestCreateVirtualGateway =
  req
    "CreateVirtualGateway"
    "fixture/CreateVirtualGateway.yaml"

requestListVirtualServices :: ListVirtualServices -> TestTree
requestListVirtualServices =
  req
    "ListVirtualServices"
    "fixture/ListVirtualServices.yaml"

requestDeleteVirtualService :: DeleteVirtualService -> TestTree
requestDeleteVirtualService =
  req
    "DeleteVirtualService"
    "fixture/DeleteVirtualService.yaml"

requestUpdateVirtualService :: UpdateVirtualService -> TestTree
requestUpdateVirtualService =
  req
    "UpdateVirtualService"
    "fixture/UpdateVirtualService.yaml"

requestUpdateVirtualGateway :: UpdateVirtualGateway -> TestTree
requestUpdateVirtualGateway =
  req
    "UpdateVirtualGateway"
    "fixture/UpdateVirtualGateway.yaml"

requestDeleteVirtualGateway :: DeleteVirtualGateway -> TestTree
requestDeleteVirtualGateway =
  req
    "DeleteVirtualGateway"
    "fixture/DeleteVirtualGateway.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute =
  req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestCreateVirtualService :: CreateVirtualService -> TestTree
requestCreateVirtualService =
  req
    "CreateVirtualService"
    "fixture/CreateVirtualService.yaml"

requestDeleteVirtualNode :: DeleteVirtualNode -> TestTree
requestDeleteVirtualNode =
  req
    "DeleteVirtualNode"
    "fixture/DeleteVirtualNode.yaml"

requestUpdateVirtualNode :: UpdateVirtualNode -> TestTree
requestUpdateVirtualNode =
  req
    "UpdateVirtualNode"
    "fixture/UpdateVirtualNode.yaml"

requestListGatewayRoutes :: ListGatewayRoutes -> TestTree
requestListGatewayRoutes =
  req
    "ListGatewayRoutes"
    "fixture/ListGatewayRoutes.yaml"

requestListRoutes :: ListRoutes -> TestTree
requestListRoutes =
  req
    "ListRoutes"
    "fixture/ListRoutes.yaml"

requestListVirtualNodes :: ListVirtualNodes -> TestTree
requestListVirtualNodes =
  req
    "ListVirtualNodes"
    "fixture/ListVirtualNodes.yaml"

requestDeleteVirtualRouter :: DeleteVirtualRouter -> TestTree
requestDeleteVirtualRouter =
  req
    "DeleteVirtualRouter"
    "fixture/DeleteVirtualRouter.yaml"

requestUpdateVirtualRouter :: UpdateVirtualRouter -> TestTree
requestUpdateVirtualRouter =
  req
    "UpdateVirtualRouter"
    "fixture/UpdateVirtualRouter.yaml"

requestCreateVirtualRouter :: CreateVirtualRouter -> TestTree
requestCreateVirtualRouter =
  req
    "CreateVirtualRouter"
    "fixture/CreateVirtualRouter.yaml"

requestDescribeVirtualService :: DescribeVirtualService -> TestTree
requestDescribeVirtualService =
  req
    "DescribeVirtualService"
    "fixture/DescribeVirtualService.yaml"

requestDescribeGatewayRoute :: DescribeGatewayRoute -> TestTree
requestDescribeGatewayRoute =
  req
    "DescribeGatewayRoute"
    "fixture/DescribeGatewayRoute.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateVirtualNode :: CreateVirtualNode -> TestTree
requestCreateVirtualNode =
  req
    "CreateVirtualNode"
    "fixture/CreateVirtualNode.yaml"

requestCreateGatewayRoute :: CreateGatewayRoute -> TestTree
requestCreateGatewayRoute =
  req
    "CreateGatewayRoute"
    "fixture/CreateGatewayRoute.yaml"

requestUpdateGatewayRoute :: UpdateGatewayRoute -> TestTree
requestUpdateGatewayRoute =
  req
    "UpdateGatewayRoute"
    "fixture/UpdateGatewayRoute.yaml"

requestDeleteGatewayRoute :: DeleteGatewayRoute -> TestTree
requestDeleteGatewayRoute =
  req
    "DeleteGatewayRoute"
    "fixture/DeleteGatewayRoute.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListVirtualGateways :: ListVirtualGateways -> TestTree
requestListVirtualGateways =
  req
    "ListVirtualGateways"
    "fixture/ListVirtualGateways.yaml"

requestListVirtualRouters :: ListVirtualRouters -> TestTree
requestListVirtualRouters =
  req
    "ListVirtualRouters"
    "fixture/ListVirtualRouters.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeMesh :: DescribeMesh -> TestTree
requestDescribeMesh =
  req
    "DescribeMesh"
    "fixture/DescribeMesh.yaml"

-- Responses

responseDescribeVirtualNode :: DescribeVirtualNodeResponse -> TestTree
responseDescribeVirtualNode =
  res
    "DescribeVirtualNodeResponse"
    "fixture/DescribeVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualNode)

responseDescribeVirtualGateway :: DescribeVirtualGatewayResponse -> TestTree
responseDescribeVirtualGateway =
  res
    "DescribeVirtualGatewayResponse"
    "fixture/DescribeVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualGateway)

responseDescribeRoute :: DescribeRouteResponse -> TestTree
responseDescribeRoute =
  res
    "DescribeRouteResponse"
    "fixture/DescribeRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoute)

responseDescribeVirtualRouter :: DescribeVirtualRouterResponse -> TestTree
responseDescribeVirtualRouter =
  res
    "DescribeVirtualRouterResponse"
    "fixture/DescribeVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualRouter)

responseListMeshes :: ListMeshesResponse -> TestTree
responseListMeshes =
  res
    "ListMeshesResponse"
    "fixture/ListMeshesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMeshes)

responseCreateMesh :: CreateMeshResponse -> TestTree
responseCreateMesh =
  res
    "CreateMeshResponse"
    "fixture/CreateMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMesh)

responseUpdateMesh :: UpdateMeshResponse -> TestTree
responseUpdateMesh =
  res
    "UpdateMeshResponse"
    "fixture/UpdateMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMesh)

responseDeleteMesh :: DeleteMeshResponse -> TestTree
responseDeleteMesh =
  res
    "DeleteMeshResponse"
    "fixture/DeleteMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMesh)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateVirtualGateway :: CreateVirtualGatewayResponse -> TestTree
responseCreateVirtualGateway =
  res
    "CreateVirtualGatewayResponse"
    "fixture/CreateVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualGateway)

responseListVirtualServices :: ListVirtualServicesResponse -> TestTree
responseListVirtualServices =
  res
    "ListVirtualServicesResponse"
    "fixture/ListVirtualServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualServices)

responseDeleteVirtualService :: DeleteVirtualServiceResponse -> TestTree
responseDeleteVirtualService =
  res
    "DeleteVirtualServiceResponse"
    "fixture/DeleteVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualService)

responseUpdateVirtualService :: UpdateVirtualServiceResponse -> TestTree
responseUpdateVirtualService =
  res
    "UpdateVirtualServiceResponse"
    "fixture/UpdateVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualService)

responseUpdateVirtualGateway :: UpdateVirtualGatewayResponse -> TestTree
responseUpdateVirtualGateway =
  res
    "UpdateVirtualGatewayResponse"
    "fixture/UpdateVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualGateway)

responseDeleteVirtualGateway :: DeleteVirtualGatewayResponse -> TestTree
responseDeleteVirtualGateway =
  res
    "DeleteVirtualGatewayResponse"
    "fixture/DeleteVirtualGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualGateway)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseUpdateRoute :: UpdateRouteResponse -> TestTree
responseUpdateRoute =
  res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoute)

responseCreateVirtualService :: CreateVirtualServiceResponse -> TestTree
responseCreateVirtualService =
  res
    "CreateVirtualServiceResponse"
    "fixture/CreateVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualService)

responseDeleteVirtualNode :: DeleteVirtualNodeResponse -> TestTree
responseDeleteVirtualNode =
  res
    "DeleteVirtualNodeResponse"
    "fixture/DeleteVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualNode)

responseUpdateVirtualNode :: UpdateVirtualNodeResponse -> TestTree
responseUpdateVirtualNode =
  res
    "UpdateVirtualNodeResponse"
    "fixture/UpdateVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualNode)

responseListGatewayRoutes :: ListGatewayRoutesResponse -> TestTree
responseListGatewayRoutes =
  res
    "ListGatewayRoutesResponse"
    "fixture/ListGatewayRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGatewayRoutes)

responseListRoutes :: ListRoutesResponse -> TestTree
responseListRoutes =
  res
    "ListRoutesResponse"
    "fixture/ListRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutes)

responseListVirtualNodes :: ListVirtualNodesResponse -> TestTree
responseListVirtualNodes =
  res
    "ListVirtualNodesResponse"
    "fixture/ListVirtualNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualNodes)

responseDeleteVirtualRouter :: DeleteVirtualRouterResponse -> TestTree
responseDeleteVirtualRouter =
  res
    "DeleteVirtualRouterResponse"
    "fixture/DeleteVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualRouter)

responseUpdateVirtualRouter :: UpdateVirtualRouterResponse -> TestTree
responseUpdateVirtualRouter =
  res
    "UpdateVirtualRouterResponse"
    "fixture/UpdateVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualRouter)

responseCreateVirtualRouter :: CreateVirtualRouterResponse -> TestTree
responseCreateVirtualRouter =
  res
    "CreateVirtualRouterResponse"
    "fixture/CreateVirtualRouterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualRouter)

responseDescribeVirtualService :: DescribeVirtualServiceResponse -> TestTree
responseDescribeVirtualService =
  res
    "DescribeVirtualServiceResponse"
    "fixture/DescribeVirtualServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualService)

responseDescribeGatewayRoute :: DescribeGatewayRouteResponse -> TestTree
responseDescribeGatewayRoute =
  res
    "DescribeGatewayRouteResponse"
    "fixture/DescribeGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayRoute)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateVirtualNode :: CreateVirtualNodeResponse -> TestTree
responseCreateVirtualNode =
  res
    "CreateVirtualNodeResponse"
    "fixture/CreateVirtualNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVirtualNode)

responseCreateGatewayRoute :: CreateGatewayRouteResponse -> TestTree
responseCreateGatewayRoute =
  res
    "CreateGatewayRouteResponse"
    "fixture/CreateGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGatewayRoute)

responseUpdateGatewayRoute :: UpdateGatewayRouteResponse -> TestTree
responseUpdateGatewayRoute =
  res
    "UpdateGatewayRouteResponse"
    "fixture/UpdateGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayRoute)

responseDeleteGatewayRoute :: DeleteGatewayRouteResponse -> TestTree
responseDeleteGatewayRoute =
  res
    "DeleteGatewayRouteResponse"
    "fixture/DeleteGatewayRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGatewayRoute)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListVirtualGateways :: ListVirtualGatewaysResponse -> TestTree
responseListVirtualGateways =
  res
    "ListVirtualGatewaysResponse"
    "fixture/ListVirtualGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualGateways)

responseListVirtualRouters :: ListVirtualRoutersResponse -> TestTree
responseListVirtualRouters =
  res
    "ListVirtualRoutersResponse"
    "fixture/ListVirtualRoutersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualRouters)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeMesh :: DescribeMeshResponse -> TestTree
responseDescribeMesh =
  res
    "DescribeMeshResponse"
    "fixture/DescribeMeshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMesh)
