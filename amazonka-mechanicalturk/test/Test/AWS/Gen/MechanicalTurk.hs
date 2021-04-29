{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MechanicalTurk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestGetQualificationScore $
--             newGetQualificationScore
--
--         , requestListReviewableHITs $
--             newListReviewableHITs
--
--         , requestListHITs $
--             newListHITs
--
--         , requestListAssignmentsForHIT $
--             newListAssignmentsForHIT
--
--         , requestListWorkersWithQualificationType $
--             newListWorkersWithQualificationType
--
--         , requestGetAccountBalance $
--             newGetAccountBalance
--
--         , requestCreateHIT $
--             newCreateHIT
--
--         , requestNotifyWorkers $
--             newNotifyWorkers
--
--         , requestListWorkerBlocks $
--             newListWorkerBlocks
--
--         , requestListHITsForQualificationType $
--             newListHITsForQualificationType
--
--         , requestCreateWorkerBlock $
--             newCreateWorkerBlock
--
--         , requestCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHIT
--
--         , requestListQualificationTypes $
--             newListQualificationTypes
--
--         , requestUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHIT
--
--         , requestGetAssignment $
--             newGetAssignment
--
--         , requestUpdateHITReviewStatus $
--             newUpdateHITReviewStatus
--
--         , requestRejectQualificationRequest $
--             newRejectQualificationRequest
--
--         , requestGetQualificationType $
--             newGetQualificationType
--
--         , requestRejectAssignment $
--             newRejectAssignment
--
--         , requestUpdateExpirationForHIT $
--             newUpdateExpirationForHIT
--
--         , requestApproveAssignment $
--             newApproveAssignment
--
--         , requestDeleteHIT $
--             newDeleteHIT
--
--         , requestListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHIT
--
--         , requestCreateHITType $
--             newCreateHITType
--
--         , requestUpdateNotificationSettings $
--             newUpdateNotificationSettings
--
--         , requestSendBonus $
--             newSendBonus
--
--         , requestListQualificationRequests $
--             newListQualificationRequests
--
--         , requestAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorker
--
--         , requestCreateHITWithHITType $
--             newCreateHITWithHITType
--
--         , requestDeleteWorkerBlock $
--             newDeleteWorkerBlock
--
--         , requestListBonusPayments $
--             newListBonusPayments
--
--         , requestDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorker
--
--         , requestSendTestEventNotification $
--             newSendTestEventNotification
--
--         , requestGetHIT $
--             newGetHIT
--
--         , requestUpdateQualificationType $
--             newUpdateQualificationType
--
--         , requestAcceptQualificationRequest $
--             newAcceptQualificationRequest
--
--         , requestDeleteQualificationType $
--             newDeleteQualificationType
--
--         , requestGetFileUploadURL $
--             newGetFileUploadURL
--
--         , requestCreateQualificationType $
--             newCreateQualificationType
--
--           ]

--     , testGroup "response"
--         [ responseGetQualificationScore $
--             newGetQualificationScoreResponse
--
--         , responseListReviewableHITs $
--             newListReviewableHITsResponse
--
--         , responseListHITs $
--             newListHITsResponse
--
--         , responseListAssignmentsForHIT $
--             newListAssignmentsForHITResponse
--
--         , responseListWorkersWithQualificationType $
--             newListWorkersWithQualificationTypeResponse
--
--         , responseGetAccountBalance $
--             newGetAccountBalanceResponse
--
--         , responseCreateHIT $
--             newCreateHITResponse
--
--         , responseNotifyWorkers $
--             newNotifyWorkersResponse
--
--         , responseListWorkerBlocks $
--             newListWorkerBlocksResponse
--
--         , responseListHITsForQualificationType $
--             newListHITsForQualificationTypeResponse
--
--         , responseCreateWorkerBlock $
--             newCreateWorkerBlockResponse
--
--         , responseCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHITResponse
--
--         , responseListQualificationTypes $
--             newListQualificationTypesResponse
--
--         , responseUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHITResponse
--
--         , responseGetAssignment $
--             newGetAssignmentResponse
--
--         , responseUpdateHITReviewStatus $
--             newUpdateHITReviewStatusResponse
--
--         , responseRejectQualificationRequest $
--             newRejectQualificationRequestResponse
--
--         , responseGetQualificationType $
--             newGetQualificationTypeResponse
--
--         , responseRejectAssignment $
--             newRejectAssignmentResponse
--
--         , responseUpdateExpirationForHIT $
--             newUpdateExpirationForHITResponse
--
--         , responseApproveAssignment $
--             newApproveAssignmentResponse
--
--         , responseDeleteHIT $
--             newDeleteHITResponse
--
--         , responseListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHITResponse
--
--         , responseCreateHITType $
--             newCreateHITTypeResponse
--
--         , responseUpdateNotificationSettings $
--             newUpdateNotificationSettingsResponse
--
--         , responseSendBonus $
--             newSendBonusResponse
--
--         , responseListQualificationRequests $
--             newListQualificationRequestsResponse
--
--         , responseAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorkerResponse
--
--         , responseCreateHITWithHITType $
--             newCreateHITWithHITTypeResponse
--
--         , responseDeleteWorkerBlock $
--             newDeleteWorkerBlockResponse
--
--         , responseListBonusPayments $
--             newListBonusPaymentsResponse
--
--         , responseDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorkerResponse
--
--         , responseSendTestEventNotification $
--             newSendTestEventNotificationResponse
--
--         , responseGetHIT $
--             newGetHITResponse
--
--         , responseUpdateQualificationType $
--             newUpdateQualificationTypeResponse
--
--         , responseAcceptQualificationRequest $
--             newAcceptQualificationRequestResponse
--
--         , responseDeleteQualificationType $
--             newDeleteQualificationTypeResponse
--
--         , responseGetFileUploadURL $
--             newGetFileUploadURLResponse
--
--         , responseCreateQualificationType $
--             newCreateQualificationTypeResponse
--
--           ]
--     ]

-- Requests

requestGetQualificationScore :: GetQualificationScore -> TestTree
requestGetQualificationScore =
  req
    "GetQualificationScore"
    "fixture/GetQualificationScore.yaml"

requestListReviewableHITs :: ListReviewableHITs -> TestTree
requestListReviewableHITs =
  req
    "ListReviewableHITs"
    "fixture/ListReviewableHITs.yaml"

requestListHITs :: ListHITs -> TestTree
requestListHITs =
  req
    "ListHITs"
    "fixture/ListHITs.yaml"

requestListAssignmentsForHIT :: ListAssignmentsForHIT -> TestTree
requestListAssignmentsForHIT =
  req
    "ListAssignmentsForHIT"
    "fixture/ListAssignmentsForHIT.yaml"

requestListWorkersWithQualificationType :: ListWorkersWithQualificationType -> TestTree
requestListWorkersWithQualificationType =
  req
    "ListWorkersWithQualificationType"
    "fixture/ListWorkersWithQualificationType.yaml"

requestGetAccountBalance :: GetAccountBalance -> TestTree
requestGetAccountBalance =
  req
    "GetAccountBalance"
    "fixture/GetAccountBalance.yaml"

requestCreateHIT :: CreateHIT -> TestTree
requestCreateHIT =
  req
    "CreateHIT"
    "fixture/CreateHIT.yaml"

requestNotifyWorkers :: NotifyWorkers -> TestTree
requestNotifyWorkers =
  req
    "NotifyWorkers"
    "fixture/NotifyWorkers.yaml"

requestListWorkerBlocks :: ListWorkerBlocks -> TestTree
requestListWorkerBlocks =
  req
    "ListWorkerBlocks"
    "fixture/ListWorkerBlocks.yaml"

requestListHITsForQualificationType :: ListHITsForQualificationType -> TestTree
requestListHITsForQualificationType =
  req
    "ListHITsForQualificationType"
    "fixture/ListHITsForQualificationType.yaml"

requestCreateWorkerBlock :: CreateWorkerBlock -> TestTree
requestCreateWorkerBlock =
  req
    "CreateWorkerBlock"
    "fixture/CreateWorkerBlock.yaml"

requestCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHIT -> TestTree
requestCreateAdditionalAssignmentsForHIT =
  req
    "CreateAdditionalAssignmentsForHIT"
    "fixture/CreateAdditionalAssignmentsForHIT.yaml"

requestListQualificationTypes :: ListQualificationTypes -> TestTree
requestListQualificationTypes =
  req
    "ListQualificationTypes"
    "fixture/ListQualificationTypes.yaml"

requestUpdateHITTypeOfHIT :: UpdateHITTypeOfHIT -> TestTree
requestUpdateHITTypeOfHIT =
  req
    "UpdateHITTypeOfHIT"
    "fixture/UpdateHITTypeOfHIT.yaml"

requestGetAssignment :: GetAssignment -> TestTree
requestGetAssignment =
  req
    "GetAssignment"
    "fixture/GetAssignment.yaml"

requestUpdateHITReviewStatus :: UpdateHITReviewStatus -> TestTree
requestUpdateHITReviewStatus =
  req
    "UpdateHITReviewStatus"
    "fixture/UpdateHITReviewStatus.yaml"

requestRejectQualificationRequest :: RejectQualificationRequest -> TestTree
requestRejectQualificationRequest =
  req
    "RejectQualificationRequest"
    "fixture/RejectQualificationRequest.yaml"

requestGetQualificationType :: GetQualificationType -> TestTree
requestGetQualificationType =
  req
    "GetQualificationType"
    "fixture/GetQualificationType.yaml"

requestRejectAssignment :: RejectAssignment -> TestTree
requestRejectAssignment =
  req
    "RejectAssignment"
    "fixture/RejectAssignment.yaml"

requestUpdateExpirationForHIT :: UpdateExpirationForHIT -> TestTree
requestUpdateExpirationForHIT =
  req
    "UpdateExpirationForHIT"
    "fixture/UpdateExpirationForHIT.yaml"

requestApproveAssignment :: ApproveAssignment -> TestTree
requestApproveAssignment =
  req
    "ApproveAssignment"
    "fixture/ApproveAssignment.yaml"

requestDeleteHIT :: DeleteHIT -> TestTree
requestDeleteHIT =
  req
    "DeleteHIT"
    "fixture/DeleteHIT.yaml"

requestListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHIT -> TestTree
requestListReviewPolicyResultsForHIT =
  req
    "ListReviewPolicyResultsForHIT"
    "fixture/ListReviewPolicyResultsForHIT.yaml"

requestCreateHITType :: CreateHITType -> TestTree
requestCreateHITType =
  req
    "CreateHITType"
    "fixture/CreateHITType.yaml"

requestUpdateNotificationSettings :: UpdateNotificationSettings -> TestTree
requestUpdateNotificationSettings =
  req
    "UpdateNotificationSettings"
    "fixture/UpdateNotificationSettings.yaml"

requestSendBonus :: SendBonus -> TestTree
requestSendBonus =
  req
    "SendBonus"
    "fixture/SendBonus.yaml"

requestListQualificationRequests :: ListQualificationRequests -> TestTree
requestListQualificationRequests =
  req
    "ListQualificationRequests"
    "fixture/ListQualificationRequests.yaml"

requestAssociateQualificationWithWorker :: AssociateQualificationWithWorker -> TestTree
requestAssociateQualificationWithWorker =
  req
    "AssociateQualificationWithWorker"
    "fixture/AssociateQualificationWithWorker.yaml"

requestCreateHITWithHITType :: CreateHITWithHITType -> TestTree
requestCreateHITWithHITType =
  req
    "CreateHITWithHITType"
    "fixture/CreateHITWithHITType.yaml"

requestDeleteWorkerBlock :: DeleteWorkerBlock -> TestTree
requestDeleteWorkerBlock =
  req
    "DeleteWorkerBlock"
    "fixture/DeleteWorkerBlock.yaml"

requestListBonusPayments :: ListBonusPayments -> TestTree
requestListBonusPayments =
  req
    "ListBonusPayments"
    "fixture/ListBonusPayments.yaml"

requestDisassociateQualificationFromWorker :: DisassociateQualificationFromWorker -> TestTree
requestDisassociateQualificationFromWorker =
  req
    "DisassociateQualificationFromWorker"
    "fixture/DisassociateQualificationFromWorker.yaml"

requestSendTestEventNotification :: SendTestEventNotification -> TestTree
requestSendTestEventNotification =
  req
    "SendTestEventNotification"
    "fixture/SendTestEventNotification.yaml"

requestGetHIT :: GetHIT -> TestTree
requestGetHIT =
  req
    "GetHIT"
    "fixture/GetHIT.yaml"

requestUpdateQualificationType :: UpdateQualificationType -> TestTree
requestUpdateQualificationType =
  req
    "UpdateQualificationType"
    "fixture/UpdateQualificationType.yaml"

requestAcceptQualificationRequest :: AcceptQualificationRequest -> TestTree
requestAcceptQualificationRequest =
  req
    "AcceptQualificationRequest"
    "fixture/AcceptQualificationRequest.yaml"

requestDeleteQualificationType :: DeleteQualificationType -> TestTree
requestDeleteQualificationType =
  req
    "DeleteQualificationType"
    "fixture/DeleteQualificationType.yaml"

requestGetFileUploadURL :: GetFileUploadURL -> TestTree
requestGetFileUploadURL =
  req
    "GetFileUploadURL"
    "fixture/GetFileUploadURL.yaml"

requestCreateQualificationType :: CreateQualificationType -> TestTree
requestCreateQualificationType =
  req
    "CreateQualificationType"
    "fixture/CreateQualificationType.yaml"

-- Responses

responseGetQualificationScore :: GetQualificationScoreResponse -> TestTree
responseGetQualificationScore =
  res
    "GetQualificationScoreResponse"
    "fixture/GetQualificationScoreResponse.proto"
    defaultService
    (Proxy :: Proxy GetQualificationScore)

responseListReviewableHITs :: ListReviewableHITsResponse -> TestTree
responseListReviewableHITs =
  res
    "ListReviewableHITsResponse"
    "fixture/ListReviewableHITsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReviewableHITs)

responseListHITs :: ListHITsResponse -> TestTree
responseListHITs =
  res
    "ListHITsResponse"
    "fixture/ListHITsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHITs)

responseListAssignmentsForHIT :: ListAssignmentsForHITResponse -> TestTree
responseListAssignmentsForHIT =
  res
    "ListAssignmentsForHITResponse"
    "fixture/ListAssignmentsForHITResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssignmentsForHIT)

responseListWorkersWithQualificationType :: ListWorkersWithQualificationTypeResponse -> TestTree
responseListWorkersWithQualificationType =
  res
    "ListWorkersWithQualificationTypeResponse"
    "fixture/ListWorkersWithQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkersWithQualificationType)

responseGetAccountBalance :: GetAccountBalanceResponse -> TestTree
responseGetAccountBalance =
  res
    "GetAccountBalanceResponse"
    "fixture/GetAccountBalanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountBalance)

responseCreateHIT :: CreateHITResponse -> TestTree
responseCreateHIT =
  res
    "CreateHITResponse"
    "fixture/CreateHITResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHIT)

responseNotifyWorkers :: NotifyWorkersResponse -> TestTree
responseNotifyWorkers =
  res
    "NotifyWorkersResponse"
    "fixture/NotifyWorkersResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyWorkers)

responseListWorkerBlocks :: ListWorkerBlocksResponse -> TestTree
responseListWorkerBlocks =
  res
    "ListWorkerBlocksResponse"
    "fixture/ListWorkerBlocksResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkerBlocks)

responseListHITsForQualificationType :: ListHITsForQualificationTypeResponse -> TestTree
responseListHITsForQualificationType =
  res
    "ListHITsForQualificationTypeResponse"
    "fixture/ListHITsForQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy ListHITsForQualificationType)

responseCreateWorkerBlock :: CreateWorkerBlockResponse -> TestTree
responseCreateWorkerBlock =
  res
    "CreateWorkerBlockResponse"
    "fixture/CreateWorkerBlockResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkerBlock)

responseCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHITResponse -> TestTree
responseCreateAdditionalAssignmentsForHIT =
  res
    "CreateAdditionalAssignmentsForHITResponse"
    "fixture/CreateAdditionalAssignmentsForHITResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAdditionalAssignmentsForHIT)

responseListQualificationTypes :: ListQualificationTypesResponse -> TestTree
responseListQualificationTypes =
  res
    "ListQualificationTypesResponse"
    "fixture/ListQualificationTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListQualificationTypes)

responseUpdateHITTypeOfHIT :: UpdateHITTypeOfHITResponse -> TestTree
responseUpdateHITTypeOfHIT =
  res
    "UpdateHITTypeOfHITResponse"
    "fixture/UpdateHITTypeOfHITResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateHITTypeOfHIT)

responseGetAssignment :: GetAssignmentResponse -> TestTree
responseGetAssignment =
  res
    "GetAssignmentResponse"
    "fixture/GetAssignmentResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssignment)

responseUpdateHITReviewStatus :: UpdateHITReviewStatusResponse -> TestTree
responseUpdateHITReviewStatus =
  res
    "UpdateHITReviewStatusResponse"
    "fixture/UpdateHITReviewStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateHITReviewStatus)

responseRejectQualificationRequest :: RejectQualificationRequestResponse -> TestTree
responseRejectQualificationRequest =
  res
    "RejectQualificationRequestResponse"
    "fixture/RejectQualificationRequestResponse.proto"
    defaultService
    (Proxy :: Proxy RejectQualificationRequest)

responseGetQualificationType :: GetQualificationTypeResponse -> TestTree
responseGetQualificationType =
  res
    "GetQualificationTypeResponse"
    "fixture/GetQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetQualificationType)

responseRejectAssignment :: RejectAssignmentResponse -> TestTree
responseRejectAssignment =
  res
    "RejectAssignmentResponse"
    "fixture/RejectAssignmentResponse.proto"
    defaultService
    (Proxy :: Proxy RejectAssignment)

responseUpdateExpirationForHIT :: UpdateExpirationForHITResponse -> TestTree
responseUpdateExpirationForHIT =
  res
    "UpdateExpirationForHITResponse"
    "fixture/UpdateExpirationForHITResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateExpirationForHIT)

responseApproveAssignment :: ApproveAssignmentResponse -> TestTree
responseApproveAssignment =
  res
    "ApproveAssignmentResponse"
    "fixture/ApproveAssignmentResponse.proto"
    defaultService
    (Proxy :: Proxy ApproveAssignment)

responseDeleteHIT :: DeleteHITResponse -> TestTree
responseDeleteHIT =
  res
    "DeleteHITResponse"
    "fixture/DeleteHITResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHIT)

responseListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHITResponse -> TestTree
responseListReviewPolicyResultsForHIT =
  res
    "ListReviewPolicyResultsForHITResponse"
    "fixture/ListReviewPolicyResultsForHITResponse.proto"
    defaultService
    (Proxy :: Proxy ListReviewPolicyResultsForHIT)

responseCreateHITType :: CreateHITTypeResponse -> TestTree
responseCreateHITType =
  res
    "CreateHITTypeResponse"
    "fixture/CreateHITTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHITType)

responseUpdateNotificationSettings :: UpdateNotificationSettingsResponse -> TestTree
responseUpdateNotificationSettings =
  res
    "UpdateNotificationSettingsResponse"
    "fixture/UpdateNotificationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotificationSettings)

responseSendBonus :: SendBonusResponse -> TestTree
responseSendBonus =
  res
    "SendBonusResponse"
    "fixture/SendBonusResponse.proto"
    defaultService
    (Proxy :: Proxy SendBonus)

responseListQualificationRequests :: ListQualificationRequestsResponse -> TestTree
responseListQualificationRequests =
  res
    "ListQualificationRequestsResponse"
    "fixture/ListQualificationRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQualificationRequests)

responseAssociateQualificationWithWorker :: AssociateQualificationWithWorkerResponse -> TestTree
responseAssociateQualificationWithWorker =
  res
    "AssociateQualificationWithWorkerResponse"
    "fixture/AssociateQualificationWithWorkerResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateQualificationWithWorker)

responseCreateHITWithHITType :: CreateHITWithHITTypeResponse -> TestTree
responseCreateHITWithHITType =
  res
    "CreateHITWithHITTypeResponse"
    "fixture/CreateHITWithHITTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHITWithHITType)

responseDeleteWorkerBlock :: DeleteWorkerBlockResponse -> TestTree
responseDeleteWorkerBlock =
  res
    "DeleteWorkerBlockResponse"
    "fixture/DeleteWorkerBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkerBlock)

responseListBonusPayments :: ListBonusPaymentsResponse -> TestTree
responseListBonusPayments =
  res
    "ListBonusPaymentsResponse"
    "fixture/ListBonusPaymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBonusPayments)

responseDisassociateQualificationFromWorker :: DisassociateQualificationFromWorkerResponse -> TestTree
responseDisassociateQualificationFromWorker =
  res
    "DisassociateQualificationFromWorkerResponse"
    "fixture/DisassociateQualificationFromWorkerResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateQualificationFromWorker)

responseSendTestEventNotification :: SendTestEventNotificationResponse -> TestTree
responseSendTestEventNotification =
  res
    "SendTestEventNotificationResponse"
    "fixture/SendTestEventNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy SendTestEventNotification)

responseGetHIT :: GetHITResponse -> TestTree
responseGetHIT =
  res
    "GetHITResponse"
    "fixture/GetHITResponse.proto"
    defaultService
    (Proxy :: Proxy GetHIT)

responseUpdateQualificationType :: UpdateQualificationTypeResponse -> TestTree
responseUpdateQualificationType =
  res
    "UpdateQualificationTypeResponse"
    "fixture/UpdateQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQualificationType)

responseAcceptQualificationRequest :: AcceptQualificationRequestResponse -> TestTree
responseAcceptQualificationRequest =
  res
    "AcceptQualificationRequestResponse"
    "fixture/AcceptQualificationRequestResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptQualificationRequest)

responseDeleteQualificationType :: DeleteQualificationTypeResponse -> TestTree
responseDeleteQualificationType =
  res
    "DeleteQualificationTypeResponse"
    "fixture/DeleteQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQualificationType)

responseGetFileUploadURL :: GetFileUploadURLResponse -> TestTree
responseGetFileUploadURL =
  res
    "GetFileUploadURLResponse"
    "fixture/GetFileUploadURLResponse.proto"
    defaultService
    (Proxy :: Proxy GetFileUploadURL)

responseCreateQualificationType :: CreateQualificationTypeResponse -> TestTree
responseCreateQualificationType =
  res
    "CreateQualificationTypeResponse"
    "fixture/CreateQualificationTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQualificationType)
