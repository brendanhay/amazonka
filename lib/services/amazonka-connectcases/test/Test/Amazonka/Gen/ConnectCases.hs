{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ConnectCases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ConnectCases where

import Amazonka.ConnectCases
import qualified Data.Proxy as Proxy
import Test.Amazonka.ConnectCases.Internal
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
--         [ requestBatchGetField $
--             newBatchGetField
--
--         , requestBatchPutFieldOptions $
--             newBatchPutFieldOptions
--
--         , requestCreateCase $
--             newCreateCase
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateField $
--             newCreateField
--
--         , requestCreateLayout $
--             newCreateLayout
--
--         , requestCreateRelatedItem $
--             newCreateRelatedItem
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestGetCase $
--             newGetCase
--
--         , requestGetCaseEventConfiguration $
--             newGetCaseEventConfiguration
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestGetLayout $
--             newGetLayout
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestListCasesForContact $
--             newListCasesForContact
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListFieldOptions $
--             newListFieldOptions
--
--         , requestListFields $
--             newListFields
--
--         , requestListLayouts $
--             newListLayouts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestPutCaseEventConfiguration $
--             newPutCaseEventConfiguration
--
--         , requestSearchCases $
--             newSearchCases
--
--         , requestSearchRelatedItems $
--             newSearchRelatedItems
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCase $
--             newUpdateCase
--
--         , requestUpdateField $
--             newUpdateField
--
--         , requestUpdateLayout $
--             newUpdateLayout
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetField $
--             newBatchGetFieldResponse
--
--         , responseBatchPutFieldOptions $
--             newBatchPutFieldOptionsResponse
--
--         , responseCreateCase $
--             newCreateCaseResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateField $
--             newCreateFieldResponse
--
--         , responseCreateLayout $
--             newCreateLayoutResponse
--
--         , responseCreateRelatedItem $
--             newCreateRelatedItemResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseGetCase $
--             newGetCaseResponse
--
--         , responseGetCaseEventConfiguration $
--             newGetCaseEventConfigurationResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseGetLayout $
--             newGetLayoutResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseListCasesForContact $
--             newListCasesForContactResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListFieldOptions $
--             newListFieldOptionsResponse
--
--         , responseListFields $
--             newListFieldsResponse
--
--         , responseListLayouts $
--             newListLayoutsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responsePutCaseEventConfiguration $
--             newPutCaseEventConfigurationResponse
--
--         , responseSearchCases $
--             newSearchCasesResponse
--
--         , responseSearchRelatedItems $
--             newSearchRelatedItemsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCase $
--             newUpdateCaseResponse
--
--         , responseUpdateField $
--             newUpdateFieldResponse
--
--         , responseUpdateLayout $
--             newUpdateLayoutResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--           ]
--     ]

-- Requests

requestBatchGetField :: BatchGetField -> TestTree
requestBatchGetField =
  req
    "BatchGetField"
    "fixture/BatchGetField.yaml"

requestBatchPutFieldOptions :: BatchPutFieldOptions -> TestTree
requestBatchPutFieldOptions =
  req
    "BatchPutFieldOptions"
    "fixture/BatchPutFieldOptions.yaml"

requestCreateCase :: CreateCase -> TestTree
requestCreateCase =
  req
    "CreateCase"
    "fixture/CreateCase.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateField :: CreateField -> TestTree
requestCreateField =
  req
    "CreateField"
    "fixture/CreateField.yaml"

requestCreateLayout :: CreateLayout -> TestTree
requestCreateLayout =
  req
    "CreateLayout"
    "fixture/CreateLayout.yaml"

requestCreateRelatedItem :: CreateRelatedItem -> TestTree
requestCreateRelatedItem =
  req
    "CreateRelatedItem"
    "fixture/CreateRelatedItem.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestGetCase :: GetCase -> TestTree
requestGetCase =
  req
    "GetCase"
    "fixture/GetCase.yaml"

requestGetCaseEventConfiguration :: GetCaseEventConfiguration -> TestTree
requestGetCaseEventConfiguration =
  req
    "GetCaseEventConfiguration"
    "fixture/GetCaseEventConfiguration.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetLayout :: GetLayout -> TestTree
requestGetLayout =
  req
    "GetLayout"
    "fixture/GetLayout.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestListCasesForContact :: ListCasesForContact -> TestTree
requestListCasesForContact =
  req
    "ListCasesForContact"
    "fixture/ListCasesForContact.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListFieldOptions :: ListFieldOptions -> TestTree
requestListFieldOptions =
  req
    "ListFieldOptions"
    "fixture/ListFieldOptions.yaml"

requestListFields :: ListFields -> TestTree
requestListFields =
  req
    "ListFields"
    "fixture/ListFields.yaml"

requestListLayouts :: ListLayouts -> TestTree
requestListLayouts =
  req
    "ListLayouts"
    "fixture/ListLayouts.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestPutCaseEventConfiguration :: PutCaseEventConfiguration -> TestTree
requestPutCaseEventConfiguration =
  req
    "PutCaseEventConfiguration"
    "fixture/PutCaseEventConfiguration.yaml"

requestSearchCases :: SearchCases -> TestTree
requestSearchCases =
  req
    "SearchCases"
    "fixture/SearchCases.yaml"

requestSearchRelatedItems :: SearchRelatedItems -> TestTree
requestSearchRelatedItems =
  req
    "SearchRelatedItems"
    "fixture/SearchRelatedItems.yaml"

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

requestUpdateCase :: UpdateCase -> TestTree
requestUpdateCase =
  req
    "UpdateCase"
    "fixture/UpdateCase.yaml"

requestUpdateField :: UpdateField -> TestTree
requestUpdateField =
  req
    "UpdateField"
    "fixture/UpdateField.yaml"

requestUpdateLayout :: UpdateLayout -> TestTree
requestUpdateLayout =
  req
    "UpdateLayout"
    "fixture/UpdateLayout.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

-- Responses

responseBatchGetField :: BatchGetFieldResponse -> TestTree
responseBatchGetField =
  res
    "BatchGetFieldResponse"
    "fixture/BatchGetFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetField)

responseBatchPutFieldOptions :: BatchPutFieldOptionsResponse -> TestTree
responseBatchPutFieldOptions =
  res
    "BatchPutFieldOptionsResponse"
    "fixture/BatchPutFieldOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutFieldOptions)

responseCreateCase :: CreateCaseResponse -> TestTree
responseCreateCase =
  res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCase)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateField :: CreateFieldResponse -> TestTree
responseCreateField =
  res
    "CreateFieldResponse"
    "fixture/CreateFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateField)

responseCreateLayout :: CreateLayoutResponse -> TestTree
responseCreateLayout =
  res
    "CreateLayoutResponse"
    "fixture/CreateLayoutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLayout)

responseCreateRelatedItem :: CreateRelatedItemResponse -> TestTree
responseCreateRelatedItem =
  res
    "CreateRelatedItemResponse"
    "fixture/CreateRelatedItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelatedItem)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplate)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseGetCase :: GetCaseResponse -> TestTree
responseGetCase =
  res
    "GetCaseResponse"
    "fixture/GetCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCase)

responseGetCaseEventConfiguration :: GetCaseEventConfigurationResponse -> TestTree
responseGetCaseEventConfiguration =
  res
    "GetCaseEventConfigurationResponse"
    "fixture/GetCaseEventConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCaseEventConfiguration)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseGetLayout :: GetLayoutResponse -> TestTree
responseGetLayout =
  res
    "GetLayoutResponse"
    "fixture/GetLayoutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLayout)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplate)

responseListCasesForContact :: ListCasesForContactResponse -> TestTree
responseListCasesForContact =
  res
    "ListCasesForContactResponse"
    "fixture/ListCasesForContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCasesForContact)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListFieldOptions :: ListFieldOptionsResponse -> TestTree
responseListFieldOptions =
  res
    "ListFieldOptionsResponse"
    "fixture/ListFieldOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFieldOptions)

responseListFields :: ListFieldsResponse -> TestTree
responseListFields =
  res
    "ListFieldsResponse"
    "fixture/ListFieldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFields)

responseListLayouts :: ListLayoutsResponse -> TestTree
responseListLayouts =
  res
    "ListLayoutsResponse"
    "fixture/ListLayoutsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLayouts)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responsePutCaseEventConfiguration :: PutCaseEventConfigurationResponse -> TestTree
responsePutCaseEventConfiguration =
  res
    "PutCaseEventConfigurationResponse"
    "fixture/PutCaseEventConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCaseEventConfiguration)

responseSearchCases :: SearchCasesResponse -> TestTree
responseSearchCases =
  res
    "SearchCasesResponse"
    "fixture/SearchCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchCases)

responseSearchRelatedItems :: SearchRelatedItemsResponse -> TestTree
responseSearchRelatedItems =
  res
    "SearchRelatedItemsResponse"
    "fixture/SearchRelatedItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchRelatedItems)

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

responseUpdateCase :: UpdateCaseResponse -> TestTree
responseUpdateCase =
  res
    "UpdateCaseResponse"
    "fixture/UpdateCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCase)

responseUpdateField :: UpdateFieldResponse -> TestTree
responseUpdateField =
  res
    "UpdateFieldResponse"
    "fixture/UpdateFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateField)

responseUpdateLayout :: UpdateLayoutResponse -> TestTree
responseUpdateLayout =
  res
    "UpdateLayoutResponse"
    "fixture/UpdateLayoutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLayout)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplate)
