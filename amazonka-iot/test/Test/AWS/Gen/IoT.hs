{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoT
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IoT where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.IoT
import Test.AWS.IoT.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testListPolicies $
--             listPolicies
--
--         , testCreatePolicy $
--             createPolicy
--
--         , testRegisterCertificate $
--             registerCertificate
--
--         , testListThingPrincipals $
--             listThingPrincipals
--
--         , testListPrincipalThings $
--             listPrincipalThings
--
--         , testGetLoggingOptions $
--             getLoggingOptions
--
--         , testCreateCertificateFromCsr $
--             createCertificateFromCsr
--
--         , testDeleteThing $
--             deleteThing
--
--         , testUpdateThing $
--             updateThing
--
--         , testCancelCertificateTransfer $
--             cancelCertificateTransfer
--
--         , testDeletePolicyVersion $
--             deletePolicyVersion
--
--         , testDisableTopicRule $
--             disableTopicRule
--
--         , testCreateTopicRule $
--             createTopicRule
--
--         , testCreatePolicyVersion $
--             createPolicyVersion
--
--         , testListCACertificates $
--             listCACertificates
--
--         , testDeleteTopicRule $
--             deleteTopicRule
--
--         , testListPrincipalPolicies $
--             listPrincipalPolicies
--
--         , testDeleteCACertificate $
--             deleteCACertificate
--
--         , testUpdateCACertificate $
--             updateCACertificate
--
--         , testListTopicRules $
--             listTopicRules
--
--         , testTransferCertificate $
--             transferCertificate
--
--         , testGetTopicRule $
--             getTopicRule
--
--         , testDescribeThing $
--             describeThing
--
--         , testDeletePolicy $
--             deletePolicy
--
--         , testListCertificates $
--             listCertificates
--
--         , testGetPolicyVersion $
--             getPolicyVersion
--
--         , testDeleteCertificate $
--             deleteCertificate
--
--         , testUpdateCertificate $
--             updateCertificate
--
--         , testDescribeCACertificate $
--             describeCACertificate
--
--         , testGetRegistrationCode $
--             getRegistrationCode
--
--         , testListCertificatesByCA $
--             listCertificatesByCA
--
--         , testAttachThingPrincipal $
--             attachThingPrincipal
--
--         , testListThings $
--             listThings
--
--         , testDetachPrincipalPolicy $
--             detachPrincipalPolicy
--
--         , testDeleteRegistrationCode $
--             deleteRegistrationCode
--
--         , testCreateThing $
--             createThing
--
--         , testDescribeCertificate $
--             describeCertificate
--
--         , testReplaceTopicRule $
--             replaceTopicRule
--
--         , testSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , testListPolicyVersions $
--             listPolicyVersions
--
--         , testCreateKeysAndCertificate $
--             createKeysAndCertificate
--
--         , testEnableTopicRule $
--             enableTopicRule
--
--         , testAcceptCertificateTransfer $
--             acceptCertificateTransfer
--
--         , testGetPolicy $
--             getPolicy
--
--         , testDescribeEndpoint $
--             describeEndpoint
--
--         , testRegisterCACertificate $
--             registerCACertificate
--
--         , testSetLoggingOptions $
--             setLoggingOptions
--
--         , testAttachPrincipalPolicy $
--             attachPrincipalPolicy
--
--         , testRejectCertificateTransfer $
--             rejectCertificateTransfer
--
--         , testDetachThingPrincipal $
--             detachThingPrincipal
--
--           ]

--     , testGroup "response"
--         [ testListPoliciesResponse $
--             listPoliciesResponse
--
--         , testCreatePolicyResponse $
--             createPolicyResponse
--
--         , testRegisterCertificateResponse $
--             registerCertificateResponse
--
--         , testListThingPrincipalsResponse $
--             listThingPrincipalsResponse
--
--         , testListPrincipalThingsResponse $
--             listPrincipalThingsResponse
--
--         , testGetLoggingOptionsResponse $
--             getLoggingOptionsResponse
--
--         , testCreateCertificateFromCsrResponse $
--             createCertificateFromCsrResponse
--
--         , testDeleteThingResponse $
--             deleteThingResponse
--
--         , testUpdateThingResponse $
--             updateThingResponse
--
--         , testCancelCertificateTransferResponse $
--             cancelCertificateTransferResponse
--
--         , testDeletePolicyVersionResponse $
--             deletePolicyVersionResponse
--
--         , testDisableTopicRuleResponse $
--             disableTopicRuleResponse
--
--         , testCreateTopicRuleResponse $
--             createTopicRuleResponse
--
--         , testCreatePolicyVersionResponse $
--             createPolicyVersionResponse
--
--         , testListCACertificatesResponse $
--             listCACertificatesResponse
--
--         , testDeleteTopicRuleResponse $
--             deleteTopicRuleResponse
--
--         , testListPrincipalPoliciesResponse $
--             listPrincipalPoliciesResponse
--
--         , testDeleteCACertificateResponse $
--             deleteCACertificateResponse
--
--         , testUpdateCACertificateResponse $
--             updateCACertificateResponse
--
--         , testListTopicRulesResponse $
--             listTopicRulesResponse
--
--         , testTransferCertificateResponse $
--             transferCertificateResponse
--
--         , testGetTopicRuleResponse $
--             getTopicRuleResponse
--
--         , testDescribeThingResponse $
--             describeThingResponse
--
--         , testDeletePolicyResponse $
--             deletePolicyResponse
--
--         , testListCertificatesResponse $
--             listCertificatesResponse
--
--         , testGetPolicyVersionResponse $
--             getPolicyVersionResponse
--
--         , testDeleteCertificateResponse $
--             deleteCertificateResponse
--
--         , testUpdateCertificateResponse $
--             updateCertificateResponse
--
--         , testDescribeCACertificateResponse $
--             describeCACertificateResponse
--
--         , testGetRegistrationCodeResponse $
--             getRegistrationCodeResponse
--
--         , testListCertificatesByCAResponse $
--             listCertificatesByCAResponse
--
--         , testAttachThingPrincipalResponse $
--             attachThingPrincipalResponse
--
--         , testListThingsResponse $
--             listThingsResponse
--
--         , testDetachPrincipalPolicyResponse $
--             detachPrincipalPolicyResponse
--
--         , testDeleteRegistrationCodeResponse $
--             deleteRegistrationCodeResponse
--
--         , testCreateThingResponse $
--             createThingResponse
--
--         , testDescribeCertificateResponse $
--             describeCertificateResponse
--
--         , testReplaceTopicRuleResponse $
--             replaceTopicRuleResponse
--
--         , testSetDefaultPolicyVersionResponse $
--             setDefaultPolicyVersionResponse
--
--         , testListPolicyVersionsResponse $
--             listPolicyVersionsResponse
--
--         , testCreateKeysAndCertificateResponse $
--             createKeysAndCertificateResponse
--
--         , testEnableTopicRuleResponse $
--             enableTopicRuleResponse
--
--         , testAcceptCertificateTransferResponse $
--             acceptCertificateTransferResponse
--
--         , testGetPolicyResponse $
--             getPolicyResponse
--
--         , testDescribeEndpointResponse $
--             describeEndpointResponse
--
--         , testRegisterCACertificateResponse $
--             registerCACertificateResponse
--
--         , testSetLoggingOptionsResponse $
--             setLoggingOptionsResponse
--
--         , testAttachPrincipalPolicyResponse $
--             attachPrincipalPolicyResponse
--
--         , testRejectCertificateTransferResponse $
--             rejectCertificateTransferResponse
--
--         , testDetachThingPrincipalResponse $
--             detachThingPrincipalResponse
--
--           ]
--     ]

-- Requests

testListPolicies :: ListPolicies -> TestTree
testListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

testCreatePolicy :: CreatePolicy -> TestTree
testCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

testRegisterCertificate :: RegisterCertificate -> TestTree
testRegisterCertificate = req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

testListThingPrincipals :: ListThingPrincipals -> TestTree
testListThingPrincipals = req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

testListPrincipalThings :: ListPrincipalThings -> TestTree
testListPrincipalThings = req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

testGetLoggingOptions :: GetLoggingOptions -> TestTree
testGetLoggingOptions = req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

testCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
testCreateCertificateFromCsr = req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

testDeleteThing :: DeleteThing -> TestTree
testDeleteThing = req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

testUpdateThing :: UpdateThing -> TestTree
testUpdateThing = req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

testCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
testCancelCertificateTransfer = req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

testDeletePolicyVersion :: DeletePolicyVersion -> TestTree
testDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

testDisableTopicRule :: DisableTopicRule -> TestTree
testDisableTopicRule = req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

testCreateTopicRule :: CreateTopicRule -> TestTree
testCreateTopicRule = req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

testCreatePolicyVersion :: CreatePolicyVersion -> TestTree
testCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

testListCACertificates :: ListCACertificates -> TestTree
testListCACertificates = req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

testDeleteTopicRule :: DeleteTopicRule -> TestTree
testDeleteTopicRule = req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

testListPrincipalPolicies :: ListPrincipalPolicies -> TestTree
testListPrincipalPolicies = req
    "ListPrincipalPolicies"
    "fixture/ListPrincipalPolicies.yaml"

testDeleteCACertificate :: DeleteCACertificate -> TestTree
testDeleteCACertificate = req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

testUpdateCACertificate :: UpdateCACertificate -> TestTree
testUpdateCACertificate = req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

testListTopicRules :: ListTopicRules -> TestTree
testListTopicRules = req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

testTransferCertificate :: TransferCertificate -> TestTree
testTransferCertificate = req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

testGetTopicRule :: GetTopicRule -> TestTree
testGetTopicRule = req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

testDescribeThing :: DescribeThing -> TestTree
testDescribeThing = req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

testListCertificates :: ListCertificates -> TestTree
testListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

testGetPolicyVersion :: GetPolicyVersion -> TestTree
testGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

testDeleteCertificate :: DeleteCertificate -> TestTree
testDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

testUpdateCertificate :: UpdateCertificate -> TestTree
testUpdateCertificate = req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

testDescribeCACertificate :: DescribeCACertificate -> TestTree
testDescribeCACertificate = req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

testGetRegistrationCode :: GetRegistrationCode -> TestTree
testGetRegistrationCode = req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

testListCertificatesByCA :: ListCertificatesByCA -> TestTree
testListCertificatesByCA = req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

testAttachThingPrincipal :: AttachThingPrincipal -> TestTree
testAttachThingPrincipal = req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

testListThings :: ListThings -> TestTree
testListThings = req
    "ListThings"
    "fixture/ListThings.yaml"

testDetachPrincipalPolicy :: DetachPrincipalPolicy -> TestTree
testDetachPrincipalPolicy = req
    "DetachPrincipalPolicy"
    "fixture/DetachPrincipalPolicy.yaml"

testDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
testDeleteRegistrationCode = req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

testCreateThing :: CreateThing -> TestTree
testCreateThing = req
    "CreateThing"
    "fixture/CreateThing.yaml"

testDescribeCertificate :: DescribeCertificate -> TestTree
testDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

testReplaceTopicRule :: ReplaceTopicRule -> TestTree
testReplaceTopicRule = req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

testSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
testSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

testListPolicyVersions :: ListPolicyVersions -> TestTree
testListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

testCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
testCreateKeysAndCertificate = req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

testEnableTopicRule :: EnableTopicRule -> TestTree
testEnableTopicRule = req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

testAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
testAcceptCertificateTransfer = req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

testGetPolicy :: GetPolicy -> TestTree
testGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

testDescribeEndpoint :: DescribeEndpoint -> TestTree
testDescribeEndpoint = req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

testRegisterCACertificate :: RegisterCACertificate -> TestTree
testRegisterCACertificate = req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

testSetLoggingOptions :: SetLoggingOptions -> TestTree
testSetLoggingOptions = req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

testAttachPrincipalPolicy :: AttachPrincipalPolicy -> TestTree
testAttachPrincipalPolicy = req
    "AttachPrincipalPolicy"
    "fixture/AttachPrincipalPolicy.yaml"

testRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
testRejectCertificateTransfer = req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

testDetachThingPrincipal :: DetachThingPrincipal -> TestTree
testDetachThingPrincipal = req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

-- Responses

testListPoliciesResponse :: ListPoliciesResponse -> TestTree
testListPoliciesResponse = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicies)

testCreatePolicyResponse :: CreatePolicyResponse -> TestTree
testCreatePolicyResponse = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicy)

testRegisterCertificateResponse :: RegisterCertificateResponse -> TestTree
testRegisterCertificateResponse = res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCertificate)

testListThingPrincipalsResponse :: ListThingPrincipalsResponse -> TestTree
testListThingPrincipalsResponse = res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingPrincipals)

testListPrincipalThingsResponse :: ListPrincipalThingsResponse -> TestTree
testListPrincipalThingsResponse = res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalThings)

testGetLoggingOptionsResponse :: GetLoggingOptionsResponse -> TestTree
testGetLoggingOptionsResponse = res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetLoggingOptions)

testCreateCertificateFromCsrResponse :: CreateCertificateFromCsrResponse -> TestTree
testCreateCertificateFromCsrResponse = res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    ioT
    (Proxy :: Proxy CreateCertificateFromCsr)

testDeleteThingResponse :: DeleteThingResponse -> TestTree
testDeleteThingResponse = res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThing)

testUpdateThingResponse :: UpdateThingResponse -> TestTree
testUpdateThingResponse = res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThing)

testCancelCertificateTransferResponse :: CancelCertificateTransferResponse -> TestTree
testCancelCertificateTransferResponse = res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy CancelCertificateTransfer)

testDeletePolicyVersionResponse :: DeletePolicyVersionResponse -> TestTree
testDeletePolicyVersionResponse = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicyVersion)

testDisableTopicRuleResponse :: DisableTopicRuleResponse -> TestTree
testDisableTopicRuleResponse = res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DisableTopicRule)

testCreateTopicRuleResponse :: CreateTopicRuleResponse -> TestTree
testCreateTopicRuleResponse = res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy CreateTopicRule)

testCreatePolicyVersionResponse :: CreatePolicyVersionResponse -> TestTree
testCreatePolicyVersionResponse = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicyVersion)

testListCACertificatesResponse :: ListCACertificatesResponse -> TestTree
testListCACertificatesResponse = res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCACertificates)

testDeleteTopicRuleResponse :: DeleteTopicRuleResponse -> TestTree
testDeleteTopicRuleResponse = res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DeleteTopicRule)

testListPrincipalPoliciesResponse :: ListPrincipalPoliciesResponse -> TestTree
testListPrincipalPoliciesResponse = res
    "ListPrincipalPoliciesResponse"
    "fixture/ListPrincipalPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalPolicies)

testDeleteCACertificateResponse :: DeleteCACertificateResponse -> TestTree
testDeleteCACertificateResponse = res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCACertificate)

testUpdateCACertificateResponse :: UpdateCACertificateResponse -> TestTree
testUpdateCACertificateResponse = res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCACertificate)

testListTopicRulesResponse :: ListTopicRulesResponse -> TestTree
testListTopicRulesResponse = res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    ioT
    (Proxy :: Proxy ListTopicRules)

testTransferCertificateResponse :: TransferCertificateResponse -> TestTree
testTransferCertificateResponse = res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    ioT
    (Proxy :: Proxy TransferCertificate)

testGetTopicRuleResponse :: GetTopicRuleResponse -> TestTree
testGetTopicRuleResponse = res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy GetTopicRule)

testDescribeThingResponse :: DescribeThingResponse -> TestTree
testDescribeThingResponse = res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThing)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicy)

testListCertificatesResponse :: ListCertificatesResponse -> TestTree
testListCertificatesResponse = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificates)

testGetPolicyVersionResponse :: GetPolicyVersionResponse -> TestTree
testGetPolicyVersionResponse = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicyVersion)

testDeleteCertificateResponse :: DeleteCertificateResponse -> TestTree
testDeleteCertificateResponse = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCertificate)

testUpdateCertificateResponse :: UpdateCertificateResponse -> TestTree
testUpdateCertificateResponse = res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCertificate)

testDescribeCACertificateResponse :: DescribeCACertificateResponse -> TestTree
testDescribeCACertificateResponse = res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCACertificate)

testGetRegistrationCodeResponse :: GetRegistrationCodeResponse -> TestTree
testGetRegistrationCodeResponse = res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy GetRegistrationCode)

testListCertificatesByCAResponse :: ListCertificatesByCAResponse -> TestTree
testListCertificatesByCAResponse = res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificatesByCA)

testAttachThingPrincipalResponse :: AttachThingPrincipalResponse -> TestTree
testAttachThingPrincipalResponse = res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy AttachThingPrincipal)

testListThingsResponse :: ListThingsResponse -> TestTree
testListThingsResponse = res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListThings)

testDetachPrincipalPolicyResponse :: DetachPrincipalPolicyResponse -> TestTree
testDetachPrincipalPolicyResponse = res
    "DetachPrincipalPolicyResponse"
    "fixture/DetachPrincipalPolicyResponse.proto"
    ioT
    (Proxy :: Proxy DetachPrincipalPolicy)

testDeleteRegistrationCodeResponse :: DeleteRegistrationCodeResponse -> TestTree
testDeleteRegistrationCodeResponse = res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRegistrationCode)

testCreateThingResponse :: CreateThingResponse -> TestTree
testCreateThingResponse = res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    ioT
    (Proxy :: Proxy CreateThing)

testDescribeCertificateResponse :: DescribeCertificateResponse -> TestTree
testDescribeCertificateResponse = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCertificate)

testReplaceTopicRuleResponse :: ReplaceTopicRuleResponse -> TestTree
testReplaceTopicRuleResponse = res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy ReplaceTopicRule)

testSetDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse -> TestTree
testSetDefaultPolicyVersionResponse = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultPolicyVersion)

testListPolicyVersionsResponse :: ListPolicyVersionsResponse -> TestTree
testListPolicyVersionsResponse = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicyVersions)

testCreateKeysAndCertificateResponse :: CreateKeysAndCertificateResponse -> TestTree
testCreateKeysAndCertificateResponse = res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    ioT
    (Proxy :: Proxy CreateKeysAndCertificate)

testEnableTopicRuleResponse :: EnableTopicRuleResponse -> TestTree
testEnableTopicRuleResponse = res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy EnableTopicRule)

testAcceptCertificateTransferResponse :: AcceptCertificateTransferResponse -> TestTree
testAcceptCertificateTransferResponse = res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy AcceptCertificateTransfer)

testGetPolicyResponse :: GetPolicyResponse -> TestTree
testGetPolicyResponse = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicy)

testDescribeEndpointResponse :: DescribeEndpointResponse -> TestTree
testDescribeEndpointResponse = res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEndpoint)

testRegisterCACertificateResponse :: RegisterCACertificateResponse -> TestTree
testRegisterCACertificateResponse = res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCACertificate)

testSetLoggingOptionsResponse :: SetLoggingOptionsResponse -> TestTree
testSetLoggingOptionsResponse = res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetLoggingOptions)

testAttachPrincipalPolicyResponse :: AttachPrincipalPolicyResponse -> TestTree
testAttachPrincipalPolicyResponse = res
    "AttachPrincipalPolicyResponse"
    "fixture/AttachPrincipalPolicyResponse.proto"
    ioT
    (Proxy :: Proxy AttachPrincipalPolicy)

testRejectCertificateTransferResponse :: RejectCertificateTransferResponse -> TestTree
testRejectCertificateTransferResponse = res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy RejectCertificateTransfer)

testDetachThingPrincipalResponse :: DetachThingPrincipalResponse -> TestTree
testDetachThingPrincipalResponse = res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy DetachThingPrincipal)
