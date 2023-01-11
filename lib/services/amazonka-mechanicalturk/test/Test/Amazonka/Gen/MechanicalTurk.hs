{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MechanicalTurk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MechanicalTurk where

import Amazonka.MechanicalTurk
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MechanicalTurk.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptQualificationRequest $
--             newAcceptQualificationRequest
--
--         , requestApproveAssignment $
--             newApproveAssignment
--
--         , requestAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorker
--
--         , requestCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHIT
--
--         , requestCreateHIT $
--             newCreateHIT
--
--         , requestCreateHITType $
--             newCreateHITType
--
--         , requestCreateHITWithHITType $
--             newCreateHITWithHITType
--
--         , requestCreateQualificationType $
--             newCreateQualificationType
--
--         , requestCreateWorkerBlock $
--             newCreateWorkerBlock
--
--         , requestDeleteHIT $
--             newDeleteHIT
--
--         , requestDeleteQualificationType $
--             newDeleteQualificationType
--
--         , requestDeleteWorkerBlock $
--             newDeleteWorkerBlock
--
--         , requestDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorker
--
--         , requestGetAccountBalance $
--             newGetAccountBalance
--
--         , requestGetAssignment $
--             newGetAssignment
--
--         , requestGetFileUploadURL $
--             newGetFileUploadURL
--
--         , requestGetHIT $
--             newGetHIT
--
--         , requestGetQualificationScore $
--             newGetQualificationScore
--
--         , requestGetQualificationType $
--             newGetQualificationType
--
--         , requestListAssignmentsForHIT $
--             newListAssignmentsForHIT
--
--         , requestListBonusPayments $
--             newListBonusPayments
--
--         , requestListHITs $
--             newListHITs
--
--         , requestListHITsForQualificationType $
--             newListHITsForQualificationType
--
--         , requestListQualificationRequests $
--             newListQualificationRequests
--
--         , requestListQualificationTypes $
--             newListQualificationTypes
--
--         , requestListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHIT
--
--         , requestListReviewableHITs $
--             newListReviewableHITs
--
--         , requestListWorkerBlocks $
--             newListWorkerBlocks
--
--         , requestListWorkersWithQualificationType $
--             newListWorkersWithQualificationType
--
--         , requestNotifyWorkers $
--             newNotifyWorkers
--
--         , requestRejectAssignment $
--             newRejectAssignment
--
--         , requestRejectQualificationRequest $
--             newRejectQualificationRequest
--
--         , requestSendBonus $
--             newSendBonus
--
--         , requestSendTestEventNotification $
--             newSendTestEventNotification
--
--         , requestUpdateExpirationForHIT $
--             newUpdateExpirationForHIT
--
--         , requestUpdateHITReviewStatus $
--             newUpdateHITReviewStatus
--
--         , requestUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHIT
--
--         , requestUpdateNotificationSettings $
--             newUpdateNotificationSettings
--
--         , requestUpdateQualificationType $
--             newUpdateQualificationType
--
--           ]

--     , testGroup "response"
--         [ responseAcceptQualificationRequest $
--             newAcceptQualificationRequestResponse
--
--         , responseApproveAssignment $
--             newApproveAssignmentResponse
--
--         , responseAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorkerResponse
--
--         , responseCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHITResponse
--
--         , responseCreateHIT $
--             newCreateHITResponse
--
--         , responseCreateHITType $
--             newCreateHITTypeResponse
--
--         , responseCreateHITWithHITType $
--             newCreateHITWithHITTypeResponse
--
--         , responseCreateQualificationType $
--             newCreateQualificationTypeResponse
--
--         , responseCreateWorkerBlock $
--             newCreateWorkerBlockResponse
--
--         , responseDeleteHIT $
--             newDeleteHITResponse
--
--         , responseDeleteQualificationType $
--             newDeleteQualificationTypeResponse
--
--         , responseDeleteWorkerBlock $
--             newDeleteWorkerBlockResponse
--
--         , responseDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorkerResponse
--
--         , responseGetAccountBalance $
--             newGetAccountBalanceResponse
--
--         , responseGetAssignment $
--             newGetAssignmentResponse
--
--         , responseGetFileUploadURL $
--             newGetFileUploadURLResponse
--
--         , responseGetHIT $
--             newGetHITResponse
--
--         , responseGetQualificationScore $
--             newGetQualificationScoreResponse
--
--         , responseGetQualificationType $
--             newGetQualificationTypeResponse
--
--         , responseListAssignmentsForHIT $
--             newListAssignmentsForHITResponse
--
--         , responseListBonusPayments $
--             newListBonusPaymentsResponse
--
--         , responseListHITs $
--             newListHITsResponse
--
--         , responseListHITsForQualificationType $
--             newListHITsForQualificationTypeResponse
--
--         , responseListQualificationRequests $
--             newListQualificationRequestsResponse
--
--         , responseListQualificationTypes $
--             newListQualificationTypesResponse
--
--         , responseListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHITResponse
--
--         , responseListReviewableHITs $
--             newListReviewableHITsResponse
--
--         , responseListWorkerBlocks $
--             newListWorkerBlocksResponse
--
--         , responseListWorkersWithQualificationType $
--             newListWorkersWithQualificationTypeResponse
--
--         , responseNotifyWorkers $
--             newNotifyWorkersResponse
--
--         , responseRejectAssignment $
--             newRejectAssignmentResponse
--
--         , responseRejectQualificationRequest $
--             newRejectQualificationRequestResponse
--
--         , responseSendBonus $
--             newSendBonusResponse
--
--         , responseSendTestEventNotification $
--             newSendTestEventNotificationResponse
--
--         , responseUpdateExpirationForHIT $
--             newUpdateExpirationForHITResponse
--
--         , responseUpdateHITReviewStatus $
--             newUpdateHITReviewStatusResponse
--
--         , responseUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHITResponse
--
--         , responseUpdateNotificationSettings $
--             newUpdateNotificationSettingsResponse
--
--         , responseUpdateQualificationType $
--             newUpdateQualificationTypeResponse
--
--           ]
--     ]

-- Requests

requestAcceptQualificationRequest :: AcceptQualificationRequest -> TestTree
requestAcceptQualificationRequest =
  req
    "AcceptQualificationRequest"
    "fixture/AcceptQualificationRequest.yaml"

requestApproveAssignment :: ApproveAssignment -> TestTree
requestApproveAssignment =
  req
    "ApproveAssignment"
    "fixture/ApproveAssignment.yaml"

requestAssociateQualificationWithWorker :: AssociateQualificationWithWorker -> TestTree
requestAssociateQualificationWithWorker =
  req
    "AssociateQualificationWithWorker"
    "fixture/AssociateQualificationWithWorker.yaml"

requestCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHIT -> TestTree
requestCreateAdditionalAssignmentsForHIT =
  req
    "CreateAdditionalAssignmentsForHIT"
    "fixture/CreateAdditionalAssignmentsForHIT.yaml"

requestCreateHIT :: CreateHIT -> TestTree
requestCreateHIT =
  req
    "CreateHIT"
    "fixture/CreateHIT.yaml"

requestCreateHITType :: CreateHITType -> TestTree
requestCreateHITType =
  req
    "CreateHITType"
    "fixture/CreateHITType.yaml"

requestCreateHITWithHITType :: CreateHITWithHITType -> TestTree
requestCreateHITWithHITType =
  req
    "CreateHITWithHITType"
    "fixture/CreateHITWithHITType.yaml"

requestCreateQualificationType :: CreateQualificationType -> TestTree
requestCreateQualificationType =
  req
    "CreateQualificationType"
    "fixture/CreateQualificationType.yaml"

requestCreateWorkerBlock :: CreateWorkerBlock -> TestTree
requestCreateWorkerBlock =
  req
    "CreateWorkerBlock"
    "fixture/CreateWorkerBlock.yaml"

requestDeleteHIT :: DeleteHIT -> TestTree
requestDeleteHIT =
  req
    "DeleteHIT"
    "fixture/DeleteHIT.yaml"

requestDeleteQualificationType :: DeleteQualificationType -> TestTree
requestDeleteQualificationType =
  req
    "DeleteQualificationType"
    "fixture/DeleteQualificationType.yaml"

requestDeleteWorkerBlock :: DeleteWorkerBlock -> TestTree
requestDeleteWorkerBlock =
  req
    "DeleteWorkerBlock"
    "fixture/DeleteWorkerBlock.yaml"

requestDisassociateQualificationFromWorker :: DisassociateQualificationFromWorker -> TestTree
requestDisassociateQualificationFromWorker =
  req
    "DisassociateQualificationFromWorker"
    "fixture/DisassociateQualificationFromWorker.yaml"

requestGetAccountBalance :: GetAccountBalance -> TestTree
requestGetAccountBalance =
  req
    "GetAccountBalance"
    "fixture/GetAccountBalance.yaml"

requestGetAssignment :: GetAssignment -> TestTree
requestGetAssignment =
  req
    "GetAssignment"
    "fixture/GetAssignment.yaml"

requestGetFileUploadURL :: GetFileUploadURL -> TestTree
requestGetFileUploadURL =
  req
    "GetFileUploadURL"
    "fixture/GetFileUploadURL.yaml"

requestGetHIT :: GetHIT -> TestTree
requestGetHIT =
  req
    "GetHIT"
    "fixture/GetHIT.yaml"

requestGetQualificationScore :: GetQualificationScore -> TestTree
requestGetQualificationScore =
  req
    "GetQualificationScore"
    "fixture/GetQualificationScore.yaml"

requestGetQualificationType :: GetQualificationType -> TestTree
requestGetQualificationType =
  req
    "GetQualificationType"
    "fixture/GetQualificationType.yaml"

requestListAssignmentsForHIT :: ListAssignmentsForHIT -> TestTree
requestListAssignmentsForHIT =
  req
    "ListAssignmentsForHIT"
    "fixture/ListAssignmentsForHIT.yaml"

requestListBonusPayments :: ListBonusPayments -> TestTree
requestListBonusPayments =
  req
    "ListBonusPayments"
    "fixture/ListBonusPayments.yaml"

requestListHITs :: ListHITs -> TestTree
requestListHITs =
  req
    "ListHITs"
    "fixture/ListHITs.yaml"

requestListHITsForQualificationType :: ListHITsForQualificationType -> TestTree
requestListHITsForQualificationType =
  req
    "ListHITsForQualificationType"
    "fixture/ListHITsForQualificationType.yaml"

requestListQualificationRequests :: ListQualificationRequests -> TestTree
requestListQualificationRequests =
  req
    "ListQualificationRequests"
    "fixture/ListQualificationRequests.yaml"

requestListQualificationTypes :: ListQualificationTypes -> TestTree
requestListQualificationTypes =
  req
    "ListQualificationTypes"
    "fixture/ListQualificationTypes.yaml"

requestListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHIT -> TestTree
requestListReviewPolicyResultsForHIT =
  req
    "ListReviewPolicyResultsForHIT"
    "fixture/ListReviewPolicyResultsForHIT.yaml"

requestListReviewableHITs :: ListReviewableHITs -> TestTree
requestListReviewableHITs =
  req
    "ListReviewableHITs"
    "fixture/ListReviewableHITs.yaml"

requestListWorkerBlocks :: ListWorkerBlocks -> TestTree
requestListWorkerBlocks =
  req
    "ListWorkerBlocks"
    "fixture/ListWorkerBlocks.yaml"

requestListWorkersWithQualificationType :: ListWorkersWithQualificationType -> TestTree
requestListWorkersWithQualificationType =
  req
    "ListWorkersWithQualificationType"
    "fixture/ListWorkersWithQualificationType.yaml"

requestNotifyWorkers :: NotifyWorkers -> TestTree
requestNotifyWorkers =
  req
    "NotifyWorkers"
    "fixture/NotifyWorkers.yaml"

requestRejectAssignment :: RejectAssignment -> TestTree
requestRejectAssignment =
  req
    "RejectAssignment"
    "fixture/RejectAssignment.yaml"

requestRejectQualificationRequest :: RejectQualificationRequest -> TestTree
requestRejectQualificationRequest =
  req
    "RejectQualificationRequest"
    "fixture/RejectQualificationRequest.yaml"

requestSendBonus :: SendBonus -> TestTree
requestSendBonus =
  req
    "SendBonus"
    "fixture/SendBonus.yaml"

requestSendTestEventNotification :: SendTestEventNotification -> TestTree
requestSendTestEventNotification =
  req
    "SendTestEventNotification"
    "fixture/SendTestEventNotification.yaml"

requestUpdateExpirationForHIT :: UpdateExpirationForHIT -> TestTree
requestUpdateExpirationForHIT =
  req
    "UpdateExpirationForHIT"
    "fixture/UpdateExpirationForHIT.yaml"

requestUpdateHITReviewStatus :: UpdateHITReviewStatus -> TestTree
requestUpdateHITReviewStatus =
  req
    "UpdateHITReviewStatus"
    "fixture/UpdateHITReviewStatus.yaml"

requestUpdateHITTypeOfHIT :: UpdateHITTypeOfHIT -> TestTree
requestUpdateHITTypeOfHIT =
  req
    "UpdateHITTypeOfHIT"
    "fixture/UpdateHITTypeOfHIT.yaml"

requestUpdateNotificationSettings :: UpdateNotificationSettings -> TestTree
requestUpdateNotificationSettings =
  req
    "UpdateNotificationSettings"
    "fixture/UpdateNotificationSettings.yaml"

requestUpdateQualificationType :: UpdateQualificationType -> TestTree
requestUpdateQualificationType =
  req
    "UpdateQualificationType"
    "fixture/UpdateQualificationType.yaml"

-- Responses

responseAcceptQualificationRequest :: AcceptQualificationRequestResponse -> TestTree
responseAcceptQualificationRequest =
  res
    "AcceptQualificationRequestResponse"
    "fixture/AcceptQualificationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptQualificationRequest)

responseApproveAssignment :: ApproveAssignmentResponse -> TestTree
responseApproveAssignment =
  res
    "ApproveAssignmentResponse"
    "fixture/ApproveAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApproveAssignment)

responseAssociateQualificationWithWorker :: AssociateQualificationWithWorkerResponse -> TestTree
responseAssociateQualificationWithWorker =
  res
    "AssociateQualificationWithWorkerResponse"
    "fixture/AssociateQualificationWithWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateQualificationWithWorker)

responseCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHITResponse -> TestTree
responseCreateAdditionalAssignmentsForHIT =
  res
    "CreateAdditionalAssignmentsForHITResponse"
    "fixture/CreateAdditionalAssignmentsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAdditionalAssignmentsForHIT)

responseCreateHIT :: CreateHITResponse -> TestTree
responseCreateHIT =
  res
    "CreateHITResponse"
    "fixture/CreateHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHIT)

responseCreateHITType :: CreateHITTypeResponse -> TestTree
responseCreateHITType =
  res
    "CreateHITTypeResponse"
    "fixture/CreateHITTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHITType)

responseCreateHITWithHITType :: CreateHITWithHITTypeResponse -> TestTree
responseCreateHITWithHITType =
  res
    "CreateHITWithHITTypeResponse"
    "fixture/CreateHITWithHITTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHITWithHITType)

responseCreateQualificationType :: CreateQualificationTypeResponse -> TestTree
responseCreateQualificationType =
  res
    "CreateQualificationTypeResponse"
    "fixture/CreateQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQualificationType)

responseCreateWorkerBlock :: CreateWorkerBlockResponse -> TestTree
responseCreateWorkerBlock =
  res
    "CreateWorkerBlockResponse"
    "fixture/CreateWorkerBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkerBlock)

responseDeleteHIT :: DeleteHITResponse -> TestTree
responseDeleteHIT =
  res
    "DeleteHITResponse"
    "fixture/DeleteHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHIT)

responseDeleteQualificationType :: DeleteQualificationTypeResponse -> TestTree
responseDeleteQualificationType =
  res
    "DeleteQualificationTypeResponse"
    "fixture/DeleteQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQualificationType)

responseDeleteWorkerBlock :: DeleteWorkerBlockResponse -> TestTree
responseDeleteWorkerBlock =
  res
    "DeleteWorkerBlockResponse"
    "fixture/DeleteWorkerBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkerBlock)

responseDisassociateQualificationFromWorker :: DisassociateQualificationFromWorkerResponse -> TestTree
responseDisassociateQualificationFromWorker =
  res
    "DisassociateQualificationFromWorkerResponse"
    "fixture/DisassociateQualificationFromWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateQualificationFromWorker)

responseGetAccountBalance :: GetAccountBalanceResponse -> TestTree
responseGetAccountBalance =
  res
    "GetAccountBalanceResponse"
    "fixture/GetAccountBalanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountBalance)

responseGetAssignment :: GetAssignmentResponse -> TestTree
responseGetAssignment =
  res
    "GetAssignmentResponse"
    "fixture/GetAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssignment)

responseGetFileUploadURL :: GetFileUploadURLResponse -> TestTree
responseGetFileUploadURL =
  res
    "GetFileUploadURLResponse"
    "fixture/GetFileUploadURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFileUploadURL)

responseGetHIT :: GetHITResponse -> TestTree
responseGetHIT =
  res
    "GetHITResponse"
    "fixture/GetHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHIT)

responseGetQualificationScore :: GetQualificationScoreResponse -> TestTree
responseGetQualificationScore =
  res
    "GetQualificationScoreResponse"
    "fixture/GetQualificationScoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQualificationScore)

responseGetQualificationType :: GetQualificationTypeResponse -> TestTree
responseGetQualificationType =
  res
    "GetQualificationTypeResponse"
    "fixture/GetQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQualificationType)

responseListAssignmentsForHIT :: ListAssignmentsForHITResponse -> TestTree
responseListAssignmentsForHIT =
  res
    "ListAssignmentsForHITResponse"
    "fixture/ListAssignmentsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssignmentsForHIT)

responseListBonusPayments :: ListBonusPaymentsResponse -> TestTree
responseListBonusPayments =
  res
    "ListBonusPaymentsResponse"
    "fixture/ListBonusPaymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBonusPayments)

responseListHITs :: ListHITsResponse -> TestTree
responseListHITs =
  res
    "ListHITsResponse"
    "fixture/ListHITsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHITs)

responseListHITsForQualificationType :: ListHITsForQualificationTypeResponse -> TestTree
responseListHITsForQualificationType =
  res
    "ListHITsForQualificationTypeResponse"
    "fixture/ListHITsForQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHITsForQualificationType)

responseListQualificationRequests :: ListQualificationRequestsResponse -> TestTree
responseListQualificationRequests =
  res
    "ListQualificationRequestsResponse"
    "fixture/ListQualificationRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQualificationRequests)

responseListQualificationTypes :: ListQualificationTypesResponse -> TestTree
responseListQualificationTypes =
  res
    "ListQualificationTypesResponse"
    "fixture/ListQualificationTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQualificationTypes)

responseListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHITResponse -> TestTree
responseListReviewPolicyResultsForHIT =
  res
    "ListReviewPolicyResultsForHITResponse"
    "fixture/ListReviewPolicyResultsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReviewPolicyResultsForHIT)

responseListReviewableHITs :: ListReviewableHITsResponse -> TestTree
responseListReviewableHITs =
  res
    "ListReviewableHITsResponse"
    "fixture/ListReviewableHITsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReviewableHITs)

responseListWorkerBlocks :: ListWorkerBlocksResponse -> TestTree
responseListWorkerBlocks =
  res
    "ListWorkerBlocksResponse"
    "fixture/ListWorkerBlocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkerBlocks)

responseListWorkersWithQualificationType :: ListWorkersWithQualificationTypeResponse -> TestTree
responseListWorkersWithQualificationType =
  res
    "ListWorkersWithQualificationTypeResponse"
    "fixture/ListWorkersWithQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkersWithQualificationType)

responseNotifyWorkers :: NotifyWorkersResponse -> TestTree
responseNotifyWorkers =
  res
    "NotifyWorkersResponse"
    "fixture/NotifyWorkersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyWorkers)

responseRejectAssignment :: RejectAssignmentResponse -> TestTree
responseRejectAssignment =
  res
    "RejectAssignmentResponse"
    "fixture/RejectAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectAssignment)

responseRejectQualificationRequest :: RejectQualificationRequestResponse -> TestTree
responseRejectQualificationRequest =
  res
    "RejectQualificationRequestResponse"
    "fixture/RejectQualificationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectQualificationRequest)

responseSendBonus :: SendBonusResponse -> TestTree
responseSendBonus =
  res
    "SendBonusResponse"
    "fixture/SendBonusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendBonus)

responseSendTestEventNotification :: SendTestEventNotificationResponse -> TestTree
responseSendTestEventNotification =
  res
    "SendTestEventNotificationResponse"
    "fixture/SendTestEventNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendTestEventNotification)

responseUpdateExpirationForHIT :: UpdateExpirationForHITResponse -> TestTree
responseUpdateExpirationForHIT =
  res
    "UpdateExpirationForHITResponse"
    "fixture/UpdateExpirationForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExpirationForHIT)

responseUpdateHITReviewStatus :: UpdateHITReviewStatusResponse -> TestTree
responseUpdateHITReviewStatus =
  res
    "UpdateHITReviewStatusResponse"
    "fixture/UpdateHITReviewStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHITReviewStatus)

responseUpdateHITTypeOfHIT :: UpdateHITTypeOfHITResponse -> TestTree
responseUpdateHITTypeOfHIT =
  res
    "UpdateHITTypeOfHITResponse"
    "fixture/UpdateHITTypeOfHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHITTypeOfHIT)

responseUpdateNotificationSettings :: UpdateNotificationSettingsResponse -> TestTree
responseUpdateNotificationSettings =
  res
    "UpdateNotificationSettingsResponse"
    "fixture/UpdateNotificationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotificationSettings)

responseUpdateQualificationType :: UpdateQualificationTypeResponse -> TestTree
responseUpdateQualificationType =
  res
    "UpdateQualificationTypeResponse"
    "fixture/UpdateQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQualificationType)
