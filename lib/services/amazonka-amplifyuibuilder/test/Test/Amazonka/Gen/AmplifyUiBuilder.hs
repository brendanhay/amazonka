{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AmplifyUiBuilder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AmplifyUiBuilder where

import Amazonka.AmplifyUiBuilder
import qualified Data.Proxy as Proxy
import Test.Amazonka.AmplifyUiBuilder.Internal
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
--         [ requestCreateComponent $
--             newCreateComponent
--
--         , requestCreateForm $
--             newCreateForm
--
--         , requestCreateTheme $
--             newCreateTheme
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestDeleteForm $
--             newDeleteForm
--
--         , requestDeleteTheme $
--             newDeleteTheme
--
--         , requestExchangeCodeForToken $
--             newExchangeCodeForToken
--
--         , requestExportComponents $
--             newExportComponents
--
--         , requestExportForms $
--             newExportForms
--
--         , requestExportThemes $
--             newExportThemes
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestGetForm $
--             newGetForm
--
--         , requestGetMetadata $
--             newGetMetadata
--
--         , requestGetTheme $
--             newGetTheme
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListForms $
--             newListForms
--
--         , requestListThemes $
--             newListThemes
--
--         , requestPutMetadataFlag $
--             newPutMetadataFlag
--
--         , requestRefreshToken $
--             newRefreshToken
--
--         , requestUpdateComponent $
--             newUpdateComponent
--
--         , requestUpdateForm $
--             newUpdateForm
--
--         , requestUpdateTheme $
--             newUpdateTheme
--
--           ]

--     , testGroup "response"
--         [ responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseCreateForm $
--             newCreateFormResponse
--
--         , responseCreateTheme $
--             newCreateThemeResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseDeleteForm $
--             newDeleteFormResponse
--
--         , responseDeleteTheme $
--             newDeleteThemeResponse
--
--         , responseExchangeCodeForToken $
--             newExchangeCodeForTokenResponse
--
--         , responseExportComponents $
--             newExportComponentsResponse
--
--         , responseExportForms $
--             newExportFormsResponse
--
--         , responseExportThemes $
--             newExportThemesResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseGetForm $
--             newGetFormResponse
--
--         , responseGetMetadata $
--             newGetMetadataResponse
--
--         , responseGetTheme $
--             newGetThemeResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListForms $
--             newListFormsResponse
--
--         , responseListThemes $
--             newListThemesResponse
--
--         , responsePutMetadataFlag $
--             newPutMetadataFlagResponse
--
--         , responseRefreshToken $
--             newRefreshTokenResponse
--
--         , responseUpdateComponent $
--             newUpdateComponentResponse
--
--         , responseUpdateForm $
--             newUpdateFormResponse
--
--         , responseUpdateTheme $
--             newUpdateThemeResponse
--
--           ]
--     ]

-- Requests

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestCreateForm :: CreateForm -> TestTree
requestCreateForm =
  req
    "CreateForm"
    "fixture/CreateForm.yaml"

requestCreateTheme :: CreateTheme -> TestTree
requestCreateTheme =
  req
    "CreateTheme"
    "fixture/CreateTheme.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestDeleteForm :: DeleteForm -> TestTree
requestDeleteForm =
  req
    "DeleteForm"
    "fixture/DeleteForm.yaml"

requestDeleteTheme :: DeleteTheme -> TestTree
requestDeleteTheme =
  req
    "DeleteTheme"
    "fixture/DeleteTheme.yaml"

requestExchangeCodeForToken :: ExchangeCodeForToken -> TestTree
requestExchangeCodeForToken =
  req
    "ExchangeCodeForToken"
    "fixture/ExchangeCodeForToken.yaml"

requestExportComponents :: ExportComponents -> TestTree
requestExportComponents =
  req
    "ExportComponents"
    "fixture/ExportComponents.yaml"

requestExportForms :: ExportForms -> TestTree
requestExportForms =
  req
    "ExportForms"
    "fixture/ExportForms.yaml"

requestExportThemes :: ExportThemes -> TestTree
requestExportThemes =
  req
    "ExportThemes"
    "fixture/ExportThemes.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestGetForm :: GetForm -> TestTree
requestGetForm =
  req
    "GetForm"
    "fixture/GetForm.yaml"

requestGetMetadata :: GetMetadata -> TestTree
requestGetMetadata =
  req
    "GetMetadata"
    "fixture/GetMetadata.yaml"

requestGetTheme :: GetTheme -> TestTree
requestGetTheme =
  req
    "GetTheme"
    "fixture/GetTheme.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListForms :: ListForms -> TestTree
requestListForms =
  req
    "ListForms"
    "fixture/ListForms.yaml"

requestListThemes :: ListThemes -> TestTree
requestListThemes =
  req
    "ListThemes"
    "fixture/ListThemes.yaml"

requestPutMetadataFlag :: PutMetadataFlag -> TestTree
requestPutMetadataFlag =
  req
    "PutMetadataFlag"
    "fixture/PutMetadataFlag.yaml"

requestRefreshToken :: RefreshToken -> TestTree
requestRefreshToken =
  req
    "RefreshToken"
    "fixture/RefreshToken.yaml"

requestUpdateComponent :: UpdateComponent -> TestTree
requestUpdateComponent =
  req
    "UpdateComponent"
    "fixture/UpdateComponent.yaml"

requestUpdateForm :: UpdateForm -> TestTree
requestUpdateForm =
  req
    "UpdateForm"
    "fixture/UpdateForm.yaml"

requestUpdateTheme :: UpdateTheme -> TestTree
requestUpdateTheme =
  req
    "UpdateTheme"
    "fixture/UpdateTheme.yaml"

-- Responses

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponent)

responseCreateForm :: CreateFormResponse -> TestTree
responseCreateForm =
  res
    "CreateFormResponse"
    "fixture/CreateFormResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateForm)

responseCreateTheme :: CreateThemeResponse -> TestTree
responseCreateTheme =
  res
    "CreateThemeResponse"
    "fixture/CreateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTheme)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseDeleteForm :: DeleteFormResponse -> TestTree
responseDeleteForm =
  res
    "DeleteFormResponse"
    "fixture/DeleteFormResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteForm)

responseDeleteTheme :: DeleteThemeResponse -> TestTree
responseDeleteTheme =
  res
    "DeleteThemeResponse"
    "fixture/DeleteThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTheme)

responseExchangeCodeForToken :: ExchangeCodeForTokenResponse -> TestTree
responseExchangeCodeForToken =
  res
    "ExchangeCodeForTokenResponse"
    "fixture/ExchangeCodeForTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExchangeCodeForToken)

responseExportComponents :: ExportComponentsResponse -> TestTree
responseExportComponents =
  res
    "ExportComponentsResponse"
    "fixture/ExportComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportComponents)

responseExportForms :: ExportFormsResponse -> TestTree
responseExportForms =
  res
    "ExportFormsResponse"
    "fixture/ExportFormsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportForms)

responseExportThemes :: ExportThemesResponse -> TestTree
responseExportThemes =
  res
    "ExportThemesResponse"
    "fixture/ExportThemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportThemes)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponent)

responseGetForm :: GetFormResponse -> TestTree
responseGetForm =
  res
    "GetFormResponse"
    "fixture/GetFormResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetForm)

responseGetMetadata :: GetMetadataResponse -> TestTree
responseGetMetadata =
  res
    "GetMetadataResponse"
    "fixture/GetMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetadata)

responseGetTheme :: GetThemeResponse -> TestTree
responseGetTheme =
  res
    "GetThemeResponse"
    "fixture/GetThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTheme)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListForms :: ListFormsResponse -> TestTree
responseListForms =
  res
    "ListFormsResponse"
    "fixture/ListFormsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListForms)

responseListThemes :: ListThemesResponse -> TestTree
responseListThemes =
  res
    "ListThemesResponse"
    "fixture/ListThemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemes)

responsePutMetadataFlag :: PutMetadataFlagResponse -> TestTree
responsePutMetadataFlag =
  res
    "PutMetadataFlagResponse"
    "fixture/PutMetadataFlagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetadataFlag)

responseRefreshToken :: RefreshTokenResponse -> TestTree
responseRefreshToken =
  res
    "RefreshTokenResponse"
    "fixture/RefreshTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RefreshToken)

responseUpdateComponent :: UpdateComponentResponse -> TestTree
responseUpdateComponent =
  res
    "UpdateComponentResponse"
    "fixture/UpdateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponent)

responseUpdateForm :: UpdateFormResponse -> TestTree
responseUpdateForm =
  res
    "UpdateFormResponse"
    "fixture/UpdateFormResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateForm)

responseUpdateTheme :: UpdateThemeResponse -> TestTree
responseUpdateTheme =
  res
    "UpdateThemeResponse"
    "fixture/UpdateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTheme)
