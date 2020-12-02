{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MechanicalTurk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MechanicalTurk where

import Data.Proxy
import Network.AWS.MechanicalTurk
import Test.AWS.Fixture
import Test.AWS.MechanicalTurk.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestApproveAssignment $
--             approveAssignment
--
--         , requestListReviewPolicyResultsForHIT $
--             listReviewPolicyResultsForHIT
--
--         , requestListHITs $
--             listHITs
--
--         , requestListWorkersWithQualificationType $
--             listWorkersWithQualificationType
--
--         , requestDeleteHIT $
--             deleteHIT
--
--         , requestListReviewableHITs $
--             listReviewableHITs
--
--         , requestGetAssignment $
--             getAssignment
--
--         , requestDeleteQualificationType $
--             deleteQualificationType
--
--         , requestUpdateQualificationType $
--             updateQualificationType
--
--         , requestListQualificationTypes $
--             listQualificationTypes
--
--         , requestUpdateHITTypeOfHIT $
--             updateHITTypeOfHIT
--
--         , requestDisassociateQualificationFromWorker $
--             disassociateQualificationFromWorker
--
--         , requestSendTestEventNotification $
--             sendTestEventNotification
--
--         , requestNotifyWorkers $
--             notifyWorkers
--
--         , requestCreateHITWithHITType $
--             createHITWithHITType
--
--         , requestCreateHITType $
--             createHITType
--
--         , requestSendBonus $
--             sendBonus
--
--         , requestListQualificationRequests $
--             listQualificationRequests
--
--         , requestUpdateExpirationForHIT $
--             updateExpirationForHIT
--
--         , requestRejectAssignment $
--             rejectAssignment
--
--         , requestListAssignmentsForHIT $
--             listAssignmentsForHIT
--
--         , requestRejectQualificationRequest $
--             rejectQualificationRequest
--
--         , requestGetQualificationScore $
--             getQualificationScore
--
--         , requestGetQualificationType $
--             getQualificationType
--
--         , requestUpdateHITReviewStatus $
--             updateHITReviewStatus
--
--         , requestCreateQualificationType $
--             createQualificationType
--
--         , requestAcceptQualificationRequest $
--             acceptQualificationRequest
--
--         , requestGetFileUploadURL $
--             getFileUploadURL
--
--         , requestCreateAdditionalAssignmentsForHIT $
--             createAdditionalAssignmentsForHIT
--
--         , requestGetHIT $
--             getHIT
--
--         , requestCreateWorkerBlock $
--             createWorkerBlock
--
--         , requestListHITsForQualificationType $
--             listHITsForQualificationType
--
--         , requestListBonusPayments $
--             listBonusPayments
--
--         , requestListWorkerBlocks $
--             listWorkerBlocks
--
--         , requestDeleteWorkerBlock $
--             deleteWorkerBlock
--
--         , requestUpdateNotificationSettings $
--             updateNotificationSettings
--
--         , requestAssociateQualificationWithWorker $
--             associateQualificationWithWorker
--
--         , requestCreateHIT $
--             createHIT
--
--         , requestGetAccountBalance $
--             getAccountBalance
--
--           ]

--     , testGroup "response"
--         [ responseApproveAssignment $
--             approveAssignmentResponse
--
--         , responseListReviewPolicyResultsForHIT $
--             listReviewPolicyResultsForHITResponse
--
--         , responseListHITs $
--             listHITsResponse
--
--         , responseListWorkersWithQualificationType $
--             listWorkersWithQualificationTypeResponse
--
--         , responseDeleteHIT $
--             deleteHITResponse
--
--         , responseListReviewableHITs $
--             listReviewableHITsResponse
--
--         , responseGetAssignment $
--             getAssignmentResponse
--
--         , responseDeleteQualificationType $
--             deleteQualificationTypeResponse
--
--         , responseUpdateQualificationType $
--             updateQualificationTypeResponse
--
--         , responseListQualificationTypes $
--             listQualificationTypesResponse
--
--         , responseUpdateHITTypeOfHIT $
--             updateHITTypeOfHITResponse
--
--         , responseDisassociateQualificationFromWorker $
--             disassociateQualificationFromWorkerResponse
--
--         , responseSendTestEventNotification $
--             sendTestEventNotificationResponse
--
--         , responseNotifyWorkers $
--             notifyWorkersResponse
--
--         , responseCreateHITWithHITType $
--             createHITWithHITTypeResponse
--
--         , responseCreateHITType $
--             createHITTypeResponse
--
--         , responseSendBonus $
--             sendBonusResponse
--
--         , responseListQualificationRequests $
--             listQualificationRequestsResponse
--
--         , responseUpdateExpirationForHIT $
--             updateExpirationForHITResponse
--
--         , responseRejectAssignment $
--             rejectAssignmentResponse
--
--         , responseListAssignmentsForHIT $
--             listAssignmentsForHITResponse
--
--         , responseRejectQualificationRequest $
--             rejectQualificationRequestResponse
--
--         , responseGetQualificationScore $
--             getQualificationScoreResponse
--
--         , responseGetQualificationType $
--             getQualificationTypeResponse
--
--         , responseUpdateHITReviewStatus $
--             updateHITReviewStatusResponse
--
--         , responseCreateQualificationType $
--             createQualificationTypeResponse
--
--         , responseAcceptQualificationRequest $
--             acceptQualificationRequestResponse
--
--         , responseGetFileUploadURL $
--             getFileUploadURLResponse
--
--         , responseCreateAdditionalAssignmentsForHIT $
--             createAdditionalAssignmentsForHITResponse
--
--         , responseGetHIT $
--             getHITResponse
--
--         , responseCreateWorkerBlock $
--             createWorkerBlockResponse
--
--         , responseListHITsForQualificationType $
--             listHITsForQualificationTypeResponse
--
--         , responseListBonusPayments $
--             listBonusPaymentsResponse
--
--         , responseListWorkerBlocks $
--             listWorkerBlocksResponse
--
--         , responseDeleteWorkerBlock $
--             deleteWorkerBlockResponse
--
--         , responseUpdateNotificationSettings $
--             updateNotificationSettingsResponse
--
--         , responseAssociateQualificationWithWorker $
--             associateQualificationWithWorkerResponse
--
--         , responseCreateHIT $
--             createHITResponse
--
--         , responseGetAccountBalance $
--             getAccountBalanceResponse
--
--           ]
--     ]

-- Requests

requestApproveAssignment :: ApproveAssignment -> TestTree
requestApproveAssignment = req
    "ApproveAssignment"
    "fixture/ApproveAssignment.yaml"

requestListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHIT -> TestTree
requestListReviewPolicyResultsForHIT = req
    "ListReviewPolicyResultsForHIT"
    "fixture/ListReviewPolicyResultsForHIT.yaml"

requestListHITs :: ListHITs -> TestTree
requestListHITs = req
    "ListHITs"
    "fixture/ListHITs.yaml"

requestListWorkersWithQualificationType :: ListWorkersWithQualificationType -> TestTree
requestListWorkersWithQualificationType = req
    "ListWorkersWithQualificationType"
    "fixture/ListWorkersWithQualificationType.yaml"

requestDeleteHIT :: DeleteHIT -> TestTree
requestDeleteHIT = req
    "DeleteHIT"
    "fixture/DeleteHIT.yaml"

requestListReviewableHITs :: ListReviewableHITs -> TestTree
requestListReviewableHITs = req
    "ListReviewableHITs"
    "fixture/ListReviewableHITs.yaml"

requestGetAssignment :: GetAssignment -> TestTree
requestGetAssignment = req
    "GetAssignment"
    "fixture/GetAssignment.yaml"

requestDeleteQualificationType :: DeleteQualificationType -> TestTree
requestDeleteQualificationType = req
    "DeleteQualificationType"
    "fixture/DeleteQualificationType.yaml"

requestUpdateQualificationType :: UpdateQualificationType -> TestTree
requestUpdateQualificationType = req
    "UpdateQualificationType"
    "fixture/UpdateQualificationType.yaml"

requestListQualificationTypes :: ListQualificationTypes -> TestTree
requestListQualificationTypes = req
    "ListQualificationTypes"
    "fixture/ListQualificationTypes.yaml"

requestUpdateHITTypeOfHIT :: UpdateHITTypeOfHIT -> TestTree
requestUpdateHITTypeOfHIT = req
    "UpdateHITTypeOfHIT"
    "fixture/UpdateHITTypeOfHIT.yaml"

requestDisassociateQualificationFromWorker :: DisassociateQualificationFromWorker -> TestTree
requestDisassociateQualificationFromWorker = req
    "DisassociateQualificationFromWorker"
    "fixture/DisassociateQualificationFromWorker.yaml"

requestSendTestEventNotification :: SendTestEventNotification -> TestTree
requestSendTestEventNotification = req
    "SendTestEventNotification"
    "fixture/SendTestEventNotification.yaml"

requestNotifyWorkers :: NotifyWorkers -> TestTree
requestNotifyWorkers = req
    "NotifyWorkers"
    "fixture/NotifyWorkers.yaml"

requestCreateHITWithHITType :: CreateHITWithHITType -> TestTree
requestCreateHITWithHITType = req
    "CreateHITWithHITType"
    "fixture/CreateHITWithHITType.yaml"

requestCreateHITType :: CreateHITType -> TestTree
requestCreateHITType = req
    "CreateHITType"
    "fixture/CreateHITType.yaml"

requestSendBonus :: SendBonus -> TestTree
requestSendBonus = req
    "SendBonus"
    "fixture/SendBonus.yaml"

requestListQualificationRequests :: ListQualificationRequests -> TestTree
requestListQualificationRequests = req
    "ListQualificationRequests"
    "fixture/ListQualificationRequests.yaml"

requestUpdateExpirationForHIT :: UpdateExpirationForHIT -> TestTree
requestUpdateExpirationForHIT = req
    "UpdateExpirationForHIT"
    "fixture/UpdateExpirationForHIT.yaml"

requestRejectAssignment :: RejectAssignment -> TestTree
requestRejectAssignment = req
    "RejectAssignment"
    "fixture/RejectAssignment.yaml"

requestListAssignmentsForHIT :: ListAssignmentsForHIT -> TestTree
requestListAssignmentsForHIT = req
    "ListAssignmentsForHIT"
    "fixture/ListAssignmentsForHIT.yaml"

requestRejectQualificationRequest :: RejectQualificationRequest -> TestTree
requestRejectQualificationRequest = req
    "RejectQualificationRequest"
    "fixture/RejectQualificationRequest.yaml"

requestGetQualificationScore :: GetQualificationScore -> TestTree
requestGetQualificationScore = req
    "GetQualificationScore"
    "fixture/GetQualificationScore.yaml"

requestGetQualificationType :: GetQualificationType -> TestTree
requestGetQualificationType = req
    "GetQualificationType"
    "fixture/GetQualificationType.yaml"

requestUpdateHITReviewStatus :: UpdateHITReviewStatus -> TestTree
requestUpdateHITReviewStatus = req
    "UpdateHITReviewStatus"
    "fixture/UpdateHITReviewStatus.yaml"

requestCreateQualificationType :: CreateQualificationType -> TestTree
requestCreateQualificationType = req
    "CreateQualificationType"
    "fixture/CreateQualificationType.yaml"

requestAcceptQualificationRequest :: AcceptQualificationRequest -> TestTree
requestAcceptQualificationRequest = req
    "AcceptQualificationRequest"
    "fixture/AcceptQualificationRequest.yaml"

requestGetFileUploadURL :: GetFileUploadURL -> TestTree
requestGetFileUploadURL = req
    "GetFileUploadURL"
    "fixture/GetFileUploadURL.yaml"

requestCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHIT -> TestTree
requestCreateAdditionalAssignmentsForHIT = req
    "CreateAdditionalAssignmentsForHIT"
    "fixture/CreateAdditionalAssignmentsForHIT.yaml"

requestGetHIT :: GetHIT -> TestTree
requestGetHIT = req
    "GetHIT"
    "fixture/GetHIT.yaml"

requestCreateWorkerBlock :: CreateWorkerBlock -> TestTree
requestCreateWorkerBlock = req
    "CreateWorkerBlock"
    "fixture/CreateWorkerBlock.yaml"

requestListHITsForQualificationType :: ListHITsForQualificationType -> TestTree
requestListHITsForQualificationType = req
    "ListHITsForQualificationType"
    "fixture/ListHITsForQualificationType.yaml"

requestListBonusPayments :: ListBonusPayments -> TestTree
requestListBonusPayments = req
    "ListBonusPayments"
    "fixture/ListBonusPayments.yaml"

requestListWorkerBlocks :: ListWorkerBlocks -> TestTree
requestListWorkerBlocks = req
    "ListWorkerBlocks"
    "fixture/ListWorkerBlocks.yaml"

requestDeleteWorkerBlock :: DeleteWorkerBlock -> TestTree
requestDeleteWorkerBlock = req
    "DeleteWorkerBlock"
    "fixture/DeleteWorkerBlock.yaml"

requestUpdateNotificationSettings :: UpdateNotificationSettings -> TestTree
requestUpdateNotificationSettings = req
    "UpdateNotificationSettings"
    "fixture/UpdateNotificationSettings.yaml"

requestAssociateQualificationWithWorker :: AssociateQualificationWithWorker -> TestTree
requestAssociateQualificationWithWorker = req
    "AssociateQualificationWithWorker"
    "fixture/AssociateQualificationWithWorker.yaml"

requestCreateHIT :: CreateHIT -> TestTree
requestCreateHIT = req
    "CreateHIT"
    "fixture/CreateHIT.yaml"

requestGetAccountBalance :: GetAccountBalance -> TestTree
requestGetAccountBalance = req
    "GetAccountBalance"
    "fixture/GetAccountBalance.yaml"

-- Responses

responseApproveAssignment :: ApproveAssignmentResponse -> TestTree
responseApproveAssignment = res
    "ApproveAssignmentResponse"
    "fixture/ApproveAssignmentResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ApproveAssignment)

responseListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHITResponse -> TestTree
responseListReviewPolicyResultsForHIT = res
    "ListReviewPolicyResultsForHITResponse"
    "fixture/ListReviewPolicyResultsForHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListReviewPolicyResultsForHIT)

responseListHITs :: ListHITsResponse -> TestTree
responseListHITs = res
    "ListHITsResponse"
    "fixture/ListHITsResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListHITs)

responseListWorkersWithQualificationType :: ListWorkersWithQualificationTypeResponse -> TestTree
responseListWorkersWithQualificationType = res
    "ListWorkersWithQualificationTypeResponse"
    "fixture/ListWorkersWithQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListWorkersWithQualificationType)

responseDeleteHIT :: DeleteHITResponse -> TestTree
responseDeleteHIT = res
    "DeleteHITResponse"
    "fixture/DeleteHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy DeleteHIT)

responseListReviewableHITs :: ListReviewableHITsResponse -> TestTree
responseListReviewableHITs = res
    "ListReviewableHITsResponse"
    "fixture/ListReviewableHITsResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListReviewableHITs)

responseGetAssignment :: GetAssignmentResponse -> TestTree
responseGetAssignment = res
    "GetAssignmentResponse"
    "fixture/GetAssignmentResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetAssignment)

responseDeleteQualificationType :: DeleteQualificationTypeResponse -> TestTree
responseDeleteQualificationType = res
    "DeleteQualificationTypeResponse"
    "fixture/DeleteQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy DeleteQualificationType)

responseUpdateQualificationType :: UpdateQualificationTypeResponse -> TestTree
responseUpdateQualificationType = res
    "UpdateQualificationTypeResponse"
    "fixture/UpdateQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy UpdateQualificationType)

responseListQualificationTypes :: ListQualificationTypesResponse -> TestTree
responseListQualificationTypes = res
    "ListQualificationTypesResponse"
    "fixture/ListQualificationTypesResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListQualificationTypes)

responseUpdateHITTypeOfHIT :: UpdateHITTypeOfHITResponse -> TestTree
responseUpdateHITTypeOfHIT = res
    "UpdateHITTypeOfHITResponse"
    "fixture/UpdateHITTypeOfHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy UpdateHITTypeOfHIT)

responseDisassociateQualificationFromWorker :: DisassociateQualificationFromWorkerResponse -> TestTree
responseDisassociateQualificationFromWorker = res
    "DisassociateQualificationFromWorkerResponse"
    "fixture/DisassociateQualificationFromWorkerResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy DisassociateQualificationFromWorker)

responseSendTestEventNotification :: SendTestEventNotificationResponse -> TestTree
responseSendTestEventNotification = res
    "SendTestEventNotificationResponse"
    "fixture/SendTestEventNotificationResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy SendTestEventNotification)

responseNotifyWorkers :: NotifyWorkersResponse -> TestTree
responseNotifyWorkers = res
    "NotifyWorkersResponse"
    "fixture/NotifyWorkersResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy NotifyWorkers)

responseCreateHITWithHITType :: CreateHITWithHITTypeResponse -> TestTree
responseCreateHITWithHITType = res
    "CreateHITWithHITTypeResponse"
    "fixture/CreateHITWithHITTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateHITWithHITType)

responseCreateHITType :: CreateHITTypeResponse -> TestTree
responseCreateHITType = res
    "CreateHITTypeResponse"
    "fixture/CreateHITTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateHITType)

responseSendBonus :: SendBonusResponse -> TestTree
responseSendBonus = res
    "SendBonusResponse"
    "fixture/SendBonusResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy SendBonus)

responseListQualificationRequests :: ListQualificationRequestsResponse -> TestTree
responseListQualificationRequests = res
    "ListQualificationRequestsResponse"
    "fixture/ListQualificationRequestsResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListQualificationRequests)

responseUpdateExpirationForHIT :: UpdateExpirationForHITResponse -> TestTree
responseUpdateExpirationForHIT = res
    "UpdateExpirationForHITResponse"
    "fixture/UpdateExpirationForHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy UpdateExpirationForHIT)

responseRejectAssignment :: RejectAssignmentResponse -> TestTree
responseRejectAssignment = res
    "RejectAssignmentResponse"
    "fixture/RejectAssignmentResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy RejectAssignment)

responseListAssignmentsForHIT :: ListAssignmentsForHITResponse -> TestTree
responseListAssignmentsForHIT = res
    "ListAssignmentsForHITResponse"
    "fixture/ListAssignmentsForHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListAssignmentsForHIT)

responseRejectQualificationRequest :: RejectQualificationRequestResponse -> TestTree
responseRejectQualificationRequest = res
    "RejectQualificationRequestResponse"
    "fixture/RejectQualificationRequestResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy RejectQualificationRequest)

responseGetQualificationScore :: GetQualificationScoreResponse -> TestTree
responseGetQualificationScore = res
    "GetQualificationScoreResponse"
    "fixture/GetQualificationScoreResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetQualificationScore)

responseGetQualificationType :: GetQualificationTypeResponse -> TestTree
responseGetQualificationType = res
    "GetQualificationTypeResponse"
    "fixture/GetQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetQualificationType)

responseUpdateHITReviewStatus :: UpdateHITReviewStatusResponse -> TestTree
responseUpdateHITReviewStatus = res
    "UpdateHITReviewStatusResponse"
    "fixture/UpdateHITReviewStatusResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy UpdateHITReviewStatus)

responseCreateQualificationType :: CreateQualificationTypeResponse -> TestTree
responseCreateQualificationType = res
    "CreateQualificationTypeResponse"
    "fixture/CreateQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateQualificationType)

responseAcceptQualificationRequest :: AcceptQualificationRequestResponse -> TestTree
responseAcceptQualificationRequest = res
    "AcceptQualificationRequestResponse"
    "fixture/AcceptQualificationRequestResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy AcceptQualificationRequest)

responseGetFileUploadURL :: GetFileUploadURLResponse -> TestTree
responseGetFileUploadURL = res
    "GetFileUploadURLResponse"
    "fixture/GetFileUploadURLResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetFileUploadURL)

responseCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHITResponse -> TestTree
responseCreateAdditionalAssignmentsForHIT = res
    "CreateAdditionalAssignmentsForHITResponse"
    "fixture/CreateAdditionalAssignmentsForHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateAdditionalAssignmentsForHIT)

responseGetHIT :: GetHITResponse -> TestTree
responseGetHIT = res
    "GetHITResponse"
    "fixture/GetHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetHIT)

responseCreateWorkerBlock :: CreateWorkerBlockResponse -> TestTree
responseCreateWorkerBlock = res
    "CreateWorkerBlockResponse"
    "fixture/CreateWorkerBlockResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateWorkerBlock)

responseListHITsForQualificationType :: ListHITsForQualificationTypeResponse -> TestTree
responseListHITsForQualificationType = res
    "ListHITsForQualificationTypeResponse"
    "fixture/ListHITsForQualificationTypeResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListHITsForQualificationType)

responseListBonusPayments :: ListBonusPaymentsResponse -> TestTree
responseListBonusPayments = res
    "ListBonusPaymentsResponse"
    "fixture/ListBonusPaymentsResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListBonusPayments)

responseListWorkerBlocks :: ListWorkerBlocksResponse -> TestTree
responseListWorkerBlocks = res
    "ListWorkerBlocksResponse"
    "fixture/ListWorkerBlocksResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy ListWorkerBlocks)

responseDeleteWorkerBlock :: DeleteWorkerBlockResponse -> TestTree
responseDeleteWorkerBlock = res
    "DeleteWorkerBlockResponse"
    "fixture/DeleteWorkerBlockResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy DeleteWorkerBlock)

responseUpdateNotificationSettings :: UpdateNotificationSettingsResponse -> TestTree
responseUpdateNotificationSettings = res
    "UpdateNotificationSettingsResponse"
    "fixture/UpdateNotificationSettingsResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy UpdateNotificationSettings)

responseAssociateQualificationWithWorker :: AssociateQualificationWithWorkerResponse -> TestTree
responseAssociateQualificationWithWorker = res
    "AssociateQualificationWithWorkerResponse"
    "fixture/AssociateQualificationWithWorkerResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy AssociateQualificationWithWorker)

responseCreateHIT :: CreateHITResponse -> TestTree
responseCreateHIT = res
    "CreateHITResponse"
    "fixture/CreateHITResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy CreateHIT)

responseGetAccountBalance :: GetAccountBalanceResponse -> TestTree
responseGetAccountBalance = res
    "GetAccountBalanceResponse"
    "fixture/GetAccountBalanceResponse.proto"
    mechanicalTurk
    (Proxy :: Proxy GetAccountBalance)
