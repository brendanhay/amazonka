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
--         [ requestListPolicies $
--             listPolicies
--
--         , requestCreatePolicy $
--             createPolicy
--
--         , requestRegisterCertificate $
--             registerCertificate
--
--         , requestListThingPrincipals $
--             listThingPrincipals
--
--         , requestListPrincipalThings $
--             listPrincipalThings
--
--         , requestGetLoggingOptions $
--             getLoggingOptions
--
--         , requestCreateCertificateFromCsr $
--             createCertificateFromCsr
--
--         , requestDeleteThing $
--             deleteThing
--
--         , requestUpdateThing $
--             updateThing
--
--         , requestCancelCertificateTransfer $
--             cancelCertificateTransfer
--
--         , requestDeletePolicyVersion $
--             deletePolicyVersion
--
--         , requestDisableTopicRule $
--             disableTopicRule
--
--         , requestCreateTopicRule $
--             createTopicRule
--
--         , requestCreatePolicyVersion $
--             createPolicyVersion
--
--         , requestListCACertificates $
--             listCACertificates
--
--         , requestDeleteTopicRule $
--             deleteTopicRule
--
--         , requestListPrincipalPolicies $
--             listPrincipalPolicies
--
--         , requestDeleteCACertificate $
--             deleteCACertificate
--
--         , requestUpdateCACertificate $
--             updateCACertificate
--
--         , requestListTopicRules $
--             listTopicRules
--
--         , requestTransferCertificate $
--             transferCertificate
--
--         , requestGetTopicRule $
--             getTopicRule
--
--         , requestDescribeThing $
--             describeThing
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestListCertificates $
--             listCertificates
--
--         , requestGetPolicyVersion $
--             getPolicyVersion
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestUpdateCertificate $
--             updateCertificate
--
--         , requestDescribeCACertificate $
--             describeCACertificate
--
--         , requestGetRegistrationCode $
--             getRegistrationCode
--
--         , requestListCertificatesByCA $
--             listCertificatesByCA
--
--         , requestAttachThingPrincipal $
--             attachThingPrincipal
--
--         , requestListThings $
--             listThings
--
--         , requestDetachPrincipalPolicy $
--             detachPrincipalPolicy
--
--         , requestDeleteRegistrationCode $
--             deleteRegistrationCode
--
--         , requestCreateThing $
--             createThing
--
--         , requestDescribeCertificate $
--             describeCertificate
--
--         , requestReplaceTopicRule $
--             replaceTopicRule
--
--         , requestSetDefaultPolicyVersion $
--             setDefaultPolicyVersion
--
--         , requestListPolicyVersions $
--             listPolicyVersions
--
--         , requestCreateKeysAndCertificate $
--             createKeysAndCertificate
--
--         , requestEnableTopicRule $
--             enableTopicRule
--
--         , requestAcceptCertificateTransfer $
--             acceptCertificateTransfer
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestDescribeEndpoint $
--             describeEndpoint
--
--         , requestRegisterCACertificate $
--             registerCACertificate
--
--         , requestSetLoggingOptions $
--             setLoggingOptions
--
--         , requestAttachPrincipalPolicy $
--             attachPrincipalPolicy
--
--         , requestRejectCertificateTransfer $
--             rejectCertificateTransfer
--
--         , requestDetachThingPrincipal $
--             detachThingPrincipal
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             listPoliciesResponse
--
--         , responseCreatePolicy $
--             createPolicyResponse
--
--         , responseRegisterCertificate $
--             registerCertificateResponse
--
--         , responseListThingPrincipals $
--             listThingPrincipalsResponse
--
--         , responseListPrincipalThings $
--             listPrincipalThingsResponse
--
--         , responseGetLoggingOptions $
--             getLoggingOptionsResponse
--
--         , responseCreateCertificateFromCsr $
--             createCertificateFromCsrResponse
--
--         , responseDeleteThing $
--             deleteThingResponse
--
--         , responseUpdateThing $
--             updateThingResponse
--
--         , responseCancelCertificateTransfer $
--             cancelCertificateTransferResponse
--
--         , responseDeletePolicyVersion $
--             deletePolicyVersionResponse
--
--         , responseDisableTopicRule $
--             disableTopicRuleResponse
--
--         , responseCreateTopicRule $
--             createTopicRuleResponse
--
--         , responseCreatePolicyVersion $
--             createPolicyVersionResponse
--
--         , responseListCACertificates $
--             listCACertificatesResponse
--
--         , responseDeleteTopicRule $
--             deleteTopicRuleResponse
--
--         , responseListPrincipalPolicies $
--             listPrincipalPoliciesResponse
--
--         , responseDeleteCACertificate $
--             deleteCACertificateResponse
--
--         , responseUpdateCACertificate $
--             updateCACertificateResponse
--
--         , responseListTopicRules $
--             listTopicRulesResponse
--
--         , responseTransferCertificate $
--             transferCertificateResponse
--
--         , responseGetTopicRule $
--             getTopicRuleResponse
--
--         , responseDescribeThing $
--             describeThingResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseListCertificates $
--             listCertificatesResponse
--
--         , responseGetPolicyVersion $
--             getPolicyVersionResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseUpdateCertificate $
--             updateCertificateResponse
--
--         , responseDescribeCACertificate $
--             describeCACertificateResponse
--
--         , responseGetRegistrationCode $
--             getRegistrationCodeResponse
--
--         , responseListCertificatesByCA $
--             listCertificatesByCAResponse
--
--         , responseAttachThingPrincipal $
--             attachThingPrincipalResponse
--
--         , responseListThings $
--             listThingsResponse
--
--         , responseDetachPrincipalPolicy $
--             detachPrincipalPolicyResponse
--
--         , responseDeleteRegistrationCode $
--             deleteRegistrationCodeResponse
--
--         , responseCreateThing $
--             createThingResponse
--
--         , responseDescribeCertificate $
--             describeCertificateResponse
--
--         , responseReplaceTopicRule $
--             replaceTopicRuleResponse
--
--         , responseSetDefaultPolicyVersion $
--             setDefaultPolicyVersionResponse
--
--         , responseListPolicyVersions $
--             listPolicyVersionsResponse
--
--         , responseCreateKeysAndCertificate $
--             createKeysAndCertificateResponse
--
--         , responseEnableTopicRule $
--             enableTopicRuleResponse
--
--         , responseAcceptCertificateTransfer $
--             acceptCertificateTransferResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseDescribeEndpoint $
--             describeEndpointResponse
--
--         , responseRegisterCACertificate $
--             registerCACertificateResponse
--
--         , responseSetLoggingOptions $
--             setLoggingOptionsResponse
--
--         , responseAttachPrincipalPolicy $
--             attachPrincipalPolicyResponse
--
--         , responseRejectCertificateTransfer $
--             rejectCertificateTransferResponse
--
--         , responseDetachThingPrincipal $
--             detachThingPrincipalResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestRegisterCertificate :: RegisterCertificate -> TestTree
requestRegisterCertificate = req
    "RegisterCertificate"
    "fixture/RegisterCertificate.yaml"

requestListThingPrincipals :: ListThingPrincipals -> TestTree
requestListThingPrincipals = req
    "ListThingPrincipals"
    "fixture/ListThingPrincipals.yaml"

requestListPrincipalThings :: ListPrincipalThings -> TestTree
requestListPrincipalThings = req
    "ListPrincipalThings"
    "fixture/ListPrincipalThings.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions = req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestCreateCertificateFromCsr :: CreateCertificateFromCsr -> TestTree
requestCreateCertificateFromCsr = req
    "CreateCertificateFromCsr"
    "fixture/CreateCertificateFromCsr.yaml"

requestDeleteThing :: DeleteThing -> TestTree
requestDeleteThing = req
    "DeleteThing"
    "fixture/DeleteThing.yaml"

requestUpdateThing :: UpdateThing -> TestTree
requestUpdateThing = req
    "UpdateThing"
    "fixture/UpdateThing.yaml"

requestCancelCertificateTransfer :: CancelCertificateTransfer -> TestTree
requestCancelCertificateTransfer = req
    "CancelCertificateTransfer"
    "fixture/CancelCertificateTransfer.yaml"

requestDeletePolicyVersion :: DeletePolicyVersion -> TestTree
requestDeletePolicyVersion = req
    "DeletePolicyVersion"
    "fixture/DeletePolicyVersion.yaml"

requestDisableTopicRule :: DisableTopicRule -> TestTree
requestDisableTopicRule = req
    "DisableTopicRule"
    "fixture/DisableTopicRule.yaml"

requestCreateTopicRule :: CreateTopicRule -> TestTree
requestCreateTopicRule = req
    "CreateTopicRule"
    "fixture/CreateTopicRule.yaml"

requestCreatePolicyVersion :: CreatePolicyVersion -> TestTree
requestCreatePolicyVersion = req
    "CreatePolicyVersion"
    "fixture/CreatePolicyVersion.yaml"

requestListCACertificates :: ListCACertificates -> TestTree
requestListCACertificates = req
    "ListCACertificates"
    "fixture/ListCACertificates.yaml"

requestDeleteTopicRule :: DeleteTopicRule -> TestTree
requestDeleteTopicRule = req
    "DeleteTopicRule"
    "fixture/DeleteTopicRule.yaml"

requestListPrincipalPolicies :: ListPrincipalPolicies -> TestTree
requestListPrincipalPolicies = req
    "ListPrincipalPolicies"
    "fixture/ListPrincipalPolicies.yaml"

requestDeleteCACertificate :: DeleteCACertificate -> TestTree
requestDeleteCACertificate = req
    "DeleteCACertificate"
    "fixture/DeleteCACertificate.yaml"

requestUpdateCACertificate :: UpdateCACertificate -> TestTree
requestUpdateCACertificate = req
    "UpdateCACertificate"
    "fixture/UpdateCACertificate.yaml"

requestListTopicRules :: ListTopicRules -> TestTree
requestListTopicRules = req
    "ListTopicRules"
    "fixture/ListTopicRules.yaml"

requestTransferCertificate :: TransferCertificate -> TestTree
requestTransferCertificate = req
    "TransferCertificate"
    "fixture/TransferCertificate.yaml"

requestGetTopicRule :: GetTopicRule -> TestTree
requestGetTopicRule = req
    "GetTopicRule"
    "fixture/GetTopicRule.yaml"

requestDescribeThing :: DescribeThing -> TestTree
requestDescribeThing = req
    "DescribeThing"
    "fixture/DescribeThing.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestListCertificates :: ListCertificates -> TestTree
requestListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

requestGetPolicyVersion :: GetPolicyVersion -> TestTree
requestGetPolicyVersion = req
    "GetPolicyVersion"
    "fixture/GetPolicyVersion.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestUpdateCertificate :: UpdateCertificate -> TestTree
requestUpdateCertificate = req
    "UpdateCertificate"
    "fixture/UpdateCertificate.yaml"

requestDescribeCACertificate :: DescribeCACertificate -> TestTree
requestDescribeCACertificate = req
    "DescribeCACertificate"
    "fixture/DescribeCACertificate.yaml"

requestGetRegistrationCode :: GetRegistrationCode -> TestTree
requestGetRegistrationCode = req
    "GetRegistrationCode"
    "fixture/GetRegistrationCode.yaml"

requestListCertificatesByCA :: ListCertificatesByCA -> TestTree
requestListCertificatesByCA = req
    "ListCertificatesByCA"
    "fixture/ListCertificatesByCA.yaml"

requestAttachThingPrincipal :: AttachThingPrincipal -> TestTree
requestAttachThingPrincipal = req
    "AttachThingPrincipal"
    "fixture/AttachThingPrincipal.yaml"

requestListThings :: ListThings -> TestTree
requestListThings = req
    "ListThings"
    "fixture/ListThings.yaml"

requestDetachPrincipalPolicy :: DetachPrincipalPolicy -> TestTree
requestDetachPrincipalPolicy = req
    "DetachPrincipalPolicy"
    "fixture/DetachPrincipalPolicy.yaml"

requestDeleteRegistrationCode :: DeleteRegistrationCode -> TestTree
requestDeleteRegistrationCode = req
    "DeleteRegistrationCode"
    "fixture/DeleteRegistrationCode.yaml"

requestCreateThing :: CreateThing -> TestTree
requestCreateThing = req
    "CreateThing"
    "fixture/CreateThing.yaml"

requestDescribeCertificate :: DescribeCertificate -> TestTree
requestDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

requestReplaceTopicRule :: ReplaceTopicRule -> TestTree
requestReplaceTopicRule = req
    "ReplaceTopicRule"
    "fixture/ReplaceTopicRule.yaml"

requestSetDefaultPolicyVersion :: SetDefaultPolicyVersion -> TestTree
requestSetDefaultPolicyVersion = req
    "SetDefaultPolicyVersion"
    "fixture/SetDefaultPolicyVersion.yaml"

requestListPolicyVersions :: ListPolicyVersions -> TestTree
requestListPolicyVersions = req
    "ListPolicyVersions"
    "fixture/ListPolicyVersions.yaml"

requestCreateKeysAndCertificate :: CreateKeysAndCertificate -> TestTree
requestCreateKeysAndCertificate = req
    "CreateKeysAndCertificate"
    "fixture/CreateKeysAndCertificate.yaml"

requestEnableTopicRule :: EnableTopicRule -> TestTree
requestEnableTopicRule = req
    "EnableTopicRule"
    "fixture/EnableTopicRule.yaml"

requestAcceptCertificateTransfer :: AcceptCertificateTransfer -> TestTree
requestAcceptCertificateTransfer = req
    "AcceptCertificateTransfer"
    "fixture/AcceptCertificateTransfer.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint = req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestRegisterCACertificate :: RegisterCACertificate -> TestTree
requestRegisterCACertificate = req
    "RegisterCACertificate"
    "fixture/RegisterCACertificate.yaml"

requestSetLoggingOptions :: SetLoggingOptions -> TestTree
requestSetLoggingOptions = req
    "SetLoggingOptions"
    "fixture/SetLoggingOptions.yaml"

requestAttachPrincipalPolicy :: AttachPrincipalPolicy -> TestTree
requestAttachPrincipalPolicy = req
    "AttachPrincipalPolicy"
    "fixture/AttachPrincipalPolicy.yaml"

requestRejectCertificateTransfer :: RejectCertificateTransfer -> TestTree
requestRejectCertificateTransfer = req
    "RejectCertificateTransfer"
    "fixture/RejectCertificateTransfer.yaml"

requestDetachThingPrincipal :: DetachThingPrincipal -> TestTree
requestDetachThingPrincipal = req
    "DetachThingPrincipal"
    "fixture/DetachThingPrincipal.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicy)

responseRegisterCertificate :: RegisterCertificateResponse -> TestTree
responseRegisterCertificate = res
    "RegisterCertificateResponse"
    "fixture/RegisterCertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCertificate)

responseListThingPrincipals :: ListThingPrincipalsResponse -> TestTree
responseListThingPrincipals = res
    "ListThingPrincipalsResponse"
    "fixture/ListThingPrincipalsResponse.proto"
    ioT
    (Proxy :: Proxy ListThingPrincipals)

responseListPrincipalThings :: ListPrincipalThingsResponse -> TestTree
responseListPrincipalThings = res
    "ListPrincipalThingsResponse"
    "fixture/ListPrincipalThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalThings)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions = res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy GetLoggingOptions)

responseCreateCertificateFromCsr :: CreateCertificateFromCsrResponse -> TestTree
responseCreateCertificateFromCsr = res
    "CreateCertificateFromCsrResponse"
    "fixture/CreateCertificateFromCsrResponse.proto"
    ioT
    (Proxy :: Proxy CreateCertificateFromCsr)

responseDeleteThing :: DeleteThingResponse -> TestTree
responseDeleteThing = res
    "DeleteThingResponse"
    "fixture/DeleteThingResponse.proto"
    ioT
    (Proxy :: Proxy DeleteThing)

responseUpdateThing :: UpdateThingResponse -> TestTree
responseUpdateThing = res
    "UpdateThingResponse"
    "fixture/UpdateThingResponse.proto"
    ioT
    (Proxy :: Proxy UpdateThing)

responseCancelCertificateTransfer :: CancelCertificateTransferResponse -> TestTree
responseCancelCertificateTransfer = res
    "CancelCertificateTransferResponse"
    "fixture/CancelCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy CancelCertificateTransfer)

responseDeletePolicyVersion :: DeletePolicyVersionResponse -> TestTree
responseDeletePolicyVersion = res
    "DeletePolicyVersionResponse"
    "fixture/DeletePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicyVersion)

responseDisableTopicRule :: DisableTopicRuleResponse -> TestTree
responseDisableTopicRule = res
    "DisableTopicRuleResponse"
    "fixture/DisableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DisableTopicRule)

responseCreateTopicRule :: CreateTopicRuleResponse -> TestTree
responseCreateTopicRule = res
    "CreateTopicRuleResponse"
    "fixture/CreateTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy CreateTopicRule)

responseCreatePolicyVersion :: CreatePolicyVersionResponse -> TestTree
responseCreatePolicyVersion = res
    "CreatePolicyVersionResponse"
    "fixture/CreatePolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy CreatePolicyVersion)

responseListCACertificates :: ListCACertificatesResponse -> TestTree
responseListCACertificates = res
    "ListCACertificatesResponse"
    "fixture/ListCACertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCACertificates)

responseDeleteTopicRule :: DeleteTopicRuleResponse -> TestTree
responseDeleteTopicRule = res
    "DeleteTopicRuleResponse"
    "fixture/DeleteTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy DeleteTopicRule)

responseListPrincipalPolicies :: ListPrincipalPoliciesResponse -> TestTree
responseListPrincipalPolicies = res
    "ListPrincipalPoliciesResponse"
    "fixture/ListPrincipalPoliciesResponse.proto"
    ioT
    (Proxy :: Proxy ListPrincipalPolicies)

responseDeleteCACertificate :: DeleteCACertificateResponse -> TestTree
responseDeleteCACertificate = res
    "DeleteCACertificateResponse"
    "fixture/DeleteCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCACertificate)

responseUpdateCACertificate :: UpdateCACertificateResponse -> TestTree
responseUpdateCACertificate = res
    "UpdateCACertificateResponse"
    "fixture/UpdateCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCACertificate)

responseListTopicRules :: ListTopicRulesResponse -> TestTree
responseListTopicRules = res
    "ListTopicRulesResponse"
    "fixture/ListTopicRulesResponse.proto"
    ioT
    (Proxy :: Proxy ListTopicRules)

responseTransferCertificate :: TransferCertificateResponse -> TestTree
responseTransferCertificate = res
    "TransferCertificateResponse"
    "fixture/TransferCertificateResponse.proto"
    ioT
    (Proxy :: Proxy TransferCertificate)

responseGetTopicRule :: GetTopicRuleResponse -> TestTree
responseGetTopicRule = res
    "GetTopicRuleResponse"
    "fixture/GetTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy GetTopicRule)

responseDescribeThing :: DescribeThingResponse -> TestTree
responseDescribeThing = res
    "DescribeThingResponse"
    "fixture/DescribeThingResponse.proto"
    ioT
    (Proxy :: Proxy DescribeThing)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    ioT
    (Proxy :: Proxy DeletePolicy)

responseListCertificates :: ListCertificatesResponse -> TestTree
responseListCertificates = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificates)

responseGetPolicyVersion :: GetPolicyVersionResponse -> TestTree
responseGetPolicyVersion = res
    "GetPolicyVersionResponse"
    "fixture/GetPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicyVersion)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DeleteCertificate)

responseUpdateCertificate :: UpdateCertificateResponse -> TestTree
responseUpdateCertificate = res
    "UpdateCertificateResponse"
    "fixture/UpdateCertificateResponse.proto"
    ioT
    (Proxy :: Proxy UpdateCertificate)

responseDescribeCACertificate :: DescribeCACertificateResponse -> TestTree
responseDescribeCACertificate = res
    "DescribeCACertificateResponse"
    "fixture/DescribeCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCACertificate)

responseGetRegistrationCode :: GetRegistrationCodeResponse -> TestTree
responseGetRegistrationCode = res
    "GetRegistrationCodeResponse"
    "fixture/GetRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy GetRegistrationCode)

responseListCertificatesByCA :: ListCertificatesByCAResponse -> TestTree
responseListCertificatesByCA = res
    "ListCertificatesByCAResponse"
    "fixture/ListCertificatesByCAResponse.proto"
    ioT
    (Proxy :: Proxy ListCertificatesByCA)

responseAttachThingPrincipal :: AttachThingPrincipalResponse -> TestTree
responseAttachThingPrincipal = res
    "AttachThingPrincipalResponse"
    "fixture/AttachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy AttachThingPrincipal)

responseListThings :: ListThingsResponse -> TestTree
responseListThings = res
    "ListThingsResponse"
    "fixture/ListThingsResponse.proto"
    ioT
    (Proxy :: Proxy ListThings)

responseDetachPrincipalPolicy :: DetachPrincipalPolicyResponse -> TestTree
responseDetachPrincipalPolicy = res
    "DetachPrincipalPolicyResponse"
    "fixture/DetachPrincipalPolicyResponse.proto"
    ioT
    (Proxy :: Proxy DetachPrincipalPolicy)

responseDeleteRegistrationCode :: DeleteRegistrationCodeResponse -> TestTree
responseDeleteRegistrationCode = res
    "DeleteRegistrationCodeResponse"
    "fixture/DeleteRegistrationCodeResponse.proto"
    ioT
    (Proxy :: Proxy DeleteRegistrationCode)

responseCreateThing :: CreateThingResponse -> TestTree
responseCreateThing = res
    "CreateThingResponse"
    "fixture/CreateThingResponse.proto"
    ioT
    (Proxy :: Proxy CreateThing)

responseDescribeCertificate :: DescribeCertificateResponse -> TestTree
responseDescribeCertificate = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    ioT
    (Proxy :: Proxy DescribeCertificate)

responseReplaceTopicRule :: ReplaceTopicRuleResponse -> TestTree
responseReplaceTopicRule = res
    "ReplaceTopicRuleResponse"
    "fixture/ReplaceTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy ReplaceTopicRule)

responseSetDefaultPolicyVersion :: SetDefaultPolicyVersionResponse -> TestTree
responseSetDefaultPolicyVersion = res
    "SetDefaultPolicyVersionResponse"
    "fixture/SetDefaultPolicyVersionResponse.proto"
    ioT
    (Proxy :: Proxy SetDefaultPolicyVersion)

responseListPolicyVersions :: ListPolicyVersionsResponse -> TestTree
responseListPolicyVersions = res
    "ListPolicyVersionsResponse"
    "fixture/ListPolicyVersionsResponse.proto"
    ioT
    (Proxy :: Proxy ListPolicyVersions)

responseCreateKeysAndCertificate :: CreateKeysAndCertificateResponse -> TestTree
responseCreateKeysAndCertificate = res
    "CreateKeysAndCertificateResponse"
    "fixture/CreateKeysAndCertificateResponse.proto"
    ioT
    (Proxy :: Proxy CreateKeysAndCertificate)

responseEnableTopicRule :: EnableTopicRuleResponse -> TestTree
responseEnableTopicRule = res
    "EnableTopicRuleResponse"
    "fixture/EnableTopicRuleResponse.proto"
    ioT
    (Proxy :: Proxy EnableTopicRule)

responseAcceptCertificateTransfer :: AcceptCertificateTransferResponse -> TestTree
responseAcceptCertificateTransfer = res
    "AcceptCertificateTransferResponse"
    "fixture/AcceptCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy AcceptCertificateTransfer)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    ioT
    (Proxy :: Proxy GetPolicy)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint = res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    ioT
    (Proxy :: Proxy DescribeEndpoint)

responseRegisterCACertificate :: RegisterCACertificateResponse -> TestTree
responseRegisterCACertificate = res
    "RegisterCACertificateResponse"
    "fixture/RegisterCACertificateResponse.proto"
    ioT
    (Proxy :: Proxy RegisterCACertificate)

responseSetLoggingOptions :: SetLoggingOptionsResponse -> TestTree
responseSetLoggingOptions = res
    "SetLoggingOptionsResponse"
    "fixture/SetLoggingOptionsResponse.proto"
    ioT
    (Proxy :: Proxy SetLoggingOptions)

responseAttachPrincipalPolicy :: AttachPrincipalPolicyResponse -> TestTree
responseAttachPrincipalPolicy = res
    "AttachPrincipalPolicyResponse"
    "fixture/AttachPrincipalPolicyResponse.proto"
    ioT
    (Proxy :: Proxy AttachPrincipalPolicy)

responseRejectCertificateTransfer :: RejectCertificateTransferResponse -> TestTree
responseRejectCertificateTransfer = res
    "RejectCertificateTransferResponse"
    "fixture/RejectCertificateTransferResponse.proto"
    ioT
    (Proxy :: Proxy RejectCertificateTransfer)

responseDetachThingPrincipal :: DetachThingPrincipalResponse -> TestTree
responseDetachThingPrincipal = res
    "DetachThingPrincipalResponse"
    "fixture/DetachThingPrincipalResponse.proto"
    ioT
    (Proxy :: Proxy DetachThingPrincipal)
