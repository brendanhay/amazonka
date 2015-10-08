{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAF
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WAF where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WAF
import Test.AWS.WAF.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testUpdateRule $
--             updateRule
--
--         , testDeleteRule $
--             deleteRule
--
--         , testCreateIPSet $
--             createIPSet
--
--         , testGetChangeTokenStatus $
--             getChangeTokenStatus
--
--         , testDeleteWebACL $
--             deleteWebACL
--
--         , testUpdateWebACL $
--             updateWebACL
--
--         , testListWebACLs $
--             listWebACLs
--
--         , testListRules $
--             listRules
--
--         , testCreateRule $
--             createRule
--
--         , testCreateWebACL $
--             createWebACL
--
--         , testListByteMatchSets $
--             listByteMatchSets
--
--         , testGetIPSet $
--             getIPSet
--
--         , testGetWebACL $
--             getWebACL
--
--         , testGetRule $
--             getRule
--
--         , testGetChangeToken $
--             getChangeToken
--
--         , testGetSampledRequests $
--             getSampledRequests
--
--         , testGetSqlInjectionMatchSet $
--             getSqlInjectionMatchSet
--
--         , testCreateSqlInjectionMatchSet $
--             createSqlInjectionMatchSet
--
--         , testCreateByteMatchSet $
--             createByteMatchSet
--
--         , testUpdateByteMatchSet $
--             updateByteMatchSet
--
--         , testDeleteByteMatchSet $
--             deleteByteMatchSet
--
--         , testDeleteIPSet $
--             deleteIPSet
--
--         , testUpdateIPSet $
--             updateIPSet
--
--         , testListIPSets $
--             listIPSets
--
--         , testGetByteMatchSet $
--             getByteMatchSet
--
--         , testListSqlInjectionMatchSets $
--             listSqlInjectionMatchSets
--
--         , testDeleteSqlInjectionMatchSet $
--             deleteSqlInjectionMatchSet
--
--         , testUpdateSqlInjectionMatchSet $
--             updateSqlInjectionMatchSet
--
--           ]

--     , testGroup "response"
--         [ testUpdateRuleResponse $
--             updateRuleResponse
--
--         , testDeleteRuleResponse $
--             deleteRuleResponse
--
--         , testCreateIPSetResponse $
--             createIPSetResponse
--
--         , testGetChangeTokenStatusResponse $
--             getChangeTokenStatusResponse
--
--         , testDeleteWebACLResponse $
--             deleteWebACLResponse
--
--         , testUpdateWebACLResponse $
--             updateWebACLResponse
--
--         , testListWebACLsResponse $
--             listWebACLsResponse
--
--         , testListRulesResponse $
--             listRulesResponse
--
--         , testCreateRuleResponse $
--             createRuleResponse
--
--         , testCreateWebACLResponse $
--             createWebACLResponse
--
--         , testListByteMatchSetsResponse $
--             listByteMatchSetsResponse
--
--         , testGetIPSetResponse $
--             getIPSetResponse
--
--         , testGetWebACLResponse $
--             getWebACLResponse
--
--         , testGetRuleResponse $
--             getRuleResponse
--
--         , testGetChangeTokenResponse $
--             getChangeTokenResponse
--
--         , testGetSampledRequestsResponse $
--             getSampledRequestsResponse
--
--         , testGetSqlInjectionMatchSetResponse $
--             getSqlInjectionMatchSetResponse
--
--         , testCreateSqlInjectionMatchSetResponse $
--             createSqlInjectionMatchSetResponse
--
--         , testCreateByteMatchSetResponse $
--             createByteMatchSetResponse
--
--         , testUpdateByteMatchSetResponse $
--             updateByteMatchSetResponse
--
--         , testDeleteByteMatchSetResponse $
--             deleteByteMatchSetResponse
--
--         , testDeleteIPSetResponse $
--             deleteIPSetResponse
--
--         , testUpdateIPSetResponse $
--             updateIPSetResponse
--
--         , testListIPSetsResponse $
--             listIPSetsResponse
--
--         , testGetByteMatchSetResponse $
--             getByteMatchSetResponse
--
--         , testListSqlInjectionMatchSetsResponse $
--             listSqlInjectionMatchSetsResponse
--
--         , testDeleteSqlInjectionMatchSetResponse $
--             deleteSqlInjectionMatchSetResponse
--
--         , testUpdateSqlInjectionMatchSetResponse $
--             updateSqlInjectionMatchSetResponse
--
--           ]
--     ]

-- Requests

testUpdateRule :: UpdateRule -> TestTree
testUpdateRule = req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

testDeleteRule :: DeleteRule -> TestTree
testDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

testCreateIPSet :: CreateIPSet -> TestTree
testCreateIPSet = req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

testGetChangeTokenStatus :: GetChangeTokenStatus -> TestTree
testGetChangeTokenStatus = req
    "GetChangeTokenStatus"
    "fixture/GetChangeTokenStatus.yaml"

testDeleteWebACL :: DeleteWebACL -> TestTree
testDeleteWebACL = req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

testUpdateWebACL :: UpdateWebACL -> TestTree
testUpdateWebACL = req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

testListWebACLs :: ListWebACLs -> TestTree
testListWebACLs = req
    "ListWebACLs"
    "fixture/ListWebACLs.yaml"

testListRules :: ListRules -> TestTree
testListRules = req
    "ListRules"
    "fixture/ListRules.yaml"

testCreateRule :: CreateRule -> TestTree
testCreateRule = req
    "CreateRule"
    "fixture/CreateRule.yaml"

testCreateWebACL :: CreateWebACL -> TestTree
testCreateWebACL = req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

testListByteMatchSets :: ListByteMatchSets -> TestTree
testListByteMatchSets = req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

testGetIPSet :: GetIPSet -> TestTree
testGetIPSet = req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

testGetWebACL :: GetWebACL -> TestTree
testGetWebACL = req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

testGetRule :: GetRule -> TestTree
testGetRule = req
    "GetRule"
    "fixture/GetRule.yaml"

testGetChangeToken :: GetChangeToken -> TestTree
testGetChangeToken = req
    "GetChangeToken"
    "fixture/GetChangeToken.yaml"

testGetSampledRequests :: GetSampledRequests -> TestTree
testGetSampledRequests = req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

testGetSqlInjectionMatchSet :: GetSqlInjectionMatchSet -> TestTree
testGetSqlInjectionMatchSet = req
    "GetSqlInjectionMatchSet"
    "fixture/GetSqlInjectionMatchSet.yaml"

testCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSet -> TestTree
testCreateSqlInjectionMatchSet = req
    "CreateSqlInjectionMatchSet"
    "fixture/CreateSqlInjectionMatchSet.yaml"

testCreateByteMatchSet :: CreateByteMatchSet -> TestTree
testCreateByteMatchSet = req
    "CreateByteMatchSet"
    "fixture/CreateByteMatchSet.yaml"

testUpdateByteMatchSet :: UpdateByteMatchSet -> TestTree
testUpdateByteMatchSet = req
    "UpdateByteMatchSet"
    "fixture/UpdateByteMatchSet.yaml"

testDeleteByteMatchSet :: DeleteByteMatchSet -> TestTree
testDeleteByteMatchSet = req
    "DeleteByteMatchSet"
    "fixture/DeleteByteMatchSet.yaml"

testDeleteIPSet :: DeleteIPSet -> TestTree
testDeleteIPSet = req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

testUpdateIPSet :: UpdateIPSet -> TestTree
testUpdateIPSet = req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

testListIPSets :: ListIPSets -> TestTree
testListIPSets = req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

testGetByteMatchSet :: GetByteMatchSet -> TestTree
testGetByteMatchSet = req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

testListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
testListSqlInjectionMatchSets = req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

testDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
testDeleteSqlInjectionMatchSet = req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

testUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSet -> TestTree
testUpdateSqlInjectionMatchSet = req
    "UpdateSqlInjectionMatchSet"
    "fixture/UpdateSqlInjectionMatchSet.yaml"

-- Responses

testUpdateRuleResponse :: UpdateRuleResponse -> TestTree
testUpdateRuleResponse = res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    wAF
    (Proxy :: Proxy UpdateRule)

testDeleteRuleResponse :: DeleteRuleResponse -> TestTree
testDeleteRuleResponse = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    wAF
    (Proxy :: Proxy DeleteRule)

testCreateIPSetResponse :: CreateIPSetResponse -> TestTree
testCreateIPSetResponse = res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    wAF
    (Proxy :: Proxy CreateIPSet)

testGetChangeTokenStatusResponse :: GetChangeTokenStatusResponse -> TestTree
testGetChangeTokenStatusResponse = res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    wAF
    (Proxy :: Proxy GetChangeTokenStatus)

testDeleteWebACLResponse :: DeleteWebACLResponse -> TestTree
testDeleteWebACLResponse = res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    wAF
    (Proxy :: Proxy DeleteWebACL)

testUpdateWebACLResponse :: UpdateWebACLResponse -> TestTree
testUpdateWebACLResponse = res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    wAF
    (Proxy :: Proxy UpdateWebACL)

testListWebACLsResponse :: ListWebACLsResponse -> TestTree
testListWebACLsResponse = res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    wAF
    (Proxy :: Proxy ListWebACLs)

testListRulesResponse :: ListRulesResponse -> TestTree
testListRulesResponse = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    wAF
    (Proxy :: Proxy ListRules)

testCreateRuleResponse :: CreateRuleResponse -> TestTree
testCreateRuleResponse = res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    wAF
    (Proxy :: Proxy CreateRule)

testCreateWebACLResponse :: CreateWebACLResponse -> TestTree
testCreateWebACLResponse = res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    wAF
    (Proxy :: Proxy CreateWebACL)

testListByteMatchSetsResponse :: ListByteMatchSetsResponse -> TestTree
testListByteMatchSetsResponse = res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    wAF
    (Proxy :: Proxy ListByteMatchSets)

testGetIPSetResponse :: GetIPSetResponse -> TestTree
testGetIPSetResponse = res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    wAF
    (Proxy :: Proxy GetIPSet)

testGetWebACLResponse :: GetWebACLResponse -> TestTree
testGetWebACLResponse = res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    wAF
    (Proxy :: Proxy GetWebACL)

testGetRuleResponse :: GetRuleResponse -> TestTree
testGetRuleResponse = res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    wAF
    (Proxy :: Proxy GetRule)

testGetChangeTokenResponse :: GetChangeTokenResponse -> TestTree
testGetChangeTokenResponse = res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    wAF
    (Proxy :: Proxy GetChangeToken)

testGetSampledRequestsResponse :: GetSampledRequestsResponse -> TestTree
testGetSampledRequestsResponse = res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    wAF
    (Proxy :: Proxy GetSampledRequests)

testGetSqlInjectionMatchSetResponse :: GetSqlInjectionMatchSetResponse -> TestTree
testGetSqlInjectionMatchSetResponse = res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy GetSqlInjectionMatchSet)

testCreateSqlInjectionMatchSetResponse :: CreateSqlInjectionMatchSetResponse -> TestTree
testCreateSqlInjectionMatchSetResponse = res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy CreateSqlInjectionMatchSet)

testCreateByteMatchSetResponse :: CreateByteMatchSetResponse -> TestTree
testCreateByteMatchSetResponse = res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy CreateByteMatchSet)

testUpdateByteMatchSetResponse :: UpdateByteMatchSetResponse -> TestTree
testUpdateByteMatchSetResponse = res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy UpdateByteMatchSet)

testDeleteByteMatchSetResponse :: DeleteByteMatchSetResponse -> TestTree
testDeleteByteMatchSetResponse = res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy DeleteByteMatchSet)

testDeleteIPSetResponse :: DeleteIPSetResponse -> TestTree
testDeleteIPSetResponse = res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    wAF
    (Proxy :: Proxy DeleteIPSet)

testUpdateIPSetResponse :: UpdateIPSetResponse -> TestTree
testUpdateIPSetResponse = res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    wAF
    (Proxy :: Proxy UpdateIPSet)

testListIPSetsResponse :: ListIPSetsResponse -> TestTree
testListIPSetsResponse = res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    wAF
    (Proxy :: Proxy ListIPSets)

testGetByteMatchSetResponse :: GetByteMatchSetResponse -> TestTree
testGetByteMatchSetResponse = res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy GetByteMatchSet)

testListSqlInjectionMatchSetsResponse :: ListSqlInjectionMatchSetsResponse -> TestTree
testListSqlInjectionMatchSetsResponse = res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    wAF
    (Proxy :: Proxy ListSqlInjectionMatchSets)

testDeleteSqlInjectionMatchSetResponse :: DeleteSqlInjectionMatchSetResponse -> TestTree
testDeleteSqlInjectionMatchSetResponse = res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy DeleteSqlInjectionMatchSet)

testUpdateSqlInjectionMatchSetResponse :: UpdateSqlInjectionMatchSetResponse -> TestTree
testUpdateSqlInjectionMatchSetResponse = res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    wAF
    (Proxy :: Proxy UpdateSqlInjectionMatchSet)
