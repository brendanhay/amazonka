{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MechanicalTurk
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestApproveAssignment $
--             newApproveAssignment
--
--         , requestListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHIT
--
--         , requestListHITs $
--             newListHITs
--
--         , requestListWorkersWithQualificationType $
--             newListWorkersWithQualificationType
--
--         , requestDeleteHIT $
--             newDeleteHIT
--
--         , requestListReviewableHITs $
--             newListReviewableHITs
--
--         , requestGetAssignment $
--             newGetAssignment
--
--         , requestDeleteQualificationType $
--             newDeleteQualificationType
--
--         , requestUpdateQualificationType $
--             newUpdateQualificationType
--
--         , requestListQualificationTypes $
--             newListQualificationTypes
--
--         , requestUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHIT
--
--         , requestDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorker
--
--         , requestSendTestEventNotification $
--             newSendTestEventNotification
--
--         , requestNotifyWorkers $
--             newNotifyWorkers
--
--         , requestCreateHITWithHITType $
--             newCreateHITWithHITType
--
--         , requestCreateHITType $
--             newCreateHITType
--
--         , requestSendBonus $
--             newSendBonus
--
--         , requestListQualificationRequests $
--             newListQualificationRequests
--
--         , requestUpdateExpirationForHIT $
--             newUpdateExpirationForHIT
--
--         , requestRejectAssignment $
--             newRejectAssignment
--
--         , requestListAssignmentsForHIT $
--             newListAssignmentsForHIT
--
--         , requestRejectQualificationRequest $
--             newRejectQualificationRequest
--
--         , requestGetQualificationScore $
--             newGetQualificationScore
--
--         , requestGetQualificationType $
--             newGetQualificationType
--
--         , requestUpdateHITReviewStatus $
--             newUpdateHITReviewStatus
--
--         , requestCreateQualificationType $
--             newCreateQualificationType
--
--         , requestAcceptQualificationRequest $
--             newAcceptQualificationRequest
--
--         , requestGetFileUploadURL $
--             newGetFileUploadURL
--
--         , requestCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHIT
--
--         , requestGetHIT $
--             newGetHIT
--
--         , requestCreateWorkerBlock $
--             newCreateWorkerBlock
--
--         , requestListHITsForQualificationType $
--             newListHITsForQualificationType
--
--         , requestListBonusPayments $
--             newListBonusPayments
--
--         , requestListWorkerBlocks $
--             newListWorkerBlocks
--
--         , requestDeleteWorkerBlock $
--             newDeleteWorkerBlock
--
--         , requestUpdateNotificationSettings $
--             newUpdateNotificationSettings
--
--         , requestAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorker
--
--         , requestCreateHIT $
--             newCreateHIT
--
--         , requestGetAccountBalance $
--             newGetAccountBalance
--
--           ]

--     , testGroup "response"
--         [ responseApproveAssignment $
--             newApproveAssignmentResponse
--
--         , responseListReviewPolicyResultsForHIT $
--             newListReviewPolicyResultsForHITResponse
--
--         , responseListHITs $
--             newListHITsResponse
--
--         , responseListWorkersWithQualificationType $
--             newListWorkersWithQualificationTypeResponse
--
--         , responseDeleteHIT $
--             newDeleteHITResponse
--
--         , responseListReviewableHITs $
--             newListReviewableHITsResponse
--
--         , responseGetAssignment $
--             newGetAssignmentResponse
--
--         , responseDeleteQualificationType $
--             newDeleteQualificationTypeResponse
--
--         , responseUpdateQualificationType $
--             newUpdateQualificationTypeResponse
--
--         , responseListQualificationTypes $
--             newListQualificationTypesResponse
--
--         , responseUpdateHITTypeOfHIT $
--             newUpdateHITTypeOfHITResponse
--
--         , responseDisassociateQualificationFromWorker $
--             newDisassociateQualificationFromWorkerResponse
--
--         , responseSendTestEventNotification $
--             newSendTestEventNotificationResponse
--
--         , responseNotifyWorkers $
--             newNotifyWorkersResponse
--
--         , responseCreateHITWithHITType $
--             newCreateHITWithHITTypeResponse
--
--         , responseCreateHITType $
--             newCreateHITTypeResponse
--
--         , responseSendBonus $
--             newSendBonusResponse
--
--         , responseListQualificationRequests $
--             newListQualificationRequestsResponse
--
--         , responseUpdateExpirationForHIT $
--             newUpdateExpirationForHITResponse
--
--         , responseRejectAssignment $
--             newRejectAssignmentResponse
--
--         , responseListAssignmentsForHIT $
--             newListAssignmentsForHITResponse
--
--         , responseRejectQualificationRequest $
--             newRejectQualificationRequestResponse
--
--         , responseGetQualificationScore $
--             newGetQualificationScoreResponse
--
--         , responseGetQualificationType $
--             newGetQualificationTypeResponse
--
--         , responseUpdateHITReviewStatus $
--             newUpdateHITReviewStatusResponse
--
--         , responseCreateQualificationType $
--             newCreateQualificationTypeResponse
--
--         , responseAcceptQualificationRequest $
--             newAcceptQualificationRequestResponse
--
--         , responseGetFileUploadURL $
--             newGetFileUploadURLResponse
--
--         , responseCreateAdditionalAssignmentsForHIT $
--             newCreateAdditionalAssignmentsForHITResponse
--
--         , responseGetHIT $
--             newGetHITResponse
--
--         , responseCreateWorkerBlock $
--             newCreateWorkerBlockResponse
--
--         , responseListHITsForQualificationType $
--             newListHITsForQualificationTypeResponse
--
--         , responseListBonusPayments $
--             newListBonusPaymentsResponse
--
--         , responseListWorkerBlocks $
--             newListWorkerBlocksResponse
--
--         , responseDeleteWorkerBlock $
--             newDeleteWorkerBlockResponse
--
--         , responseUpdateNotificationSettings $
--             newUpdateNotificationSettingsResponse
--
--         , responseAssociateQualificationWithWorker $
--             newAssociateQualificationWithWorkerResponse
--
--         , responseCreateHIT $
--             newCreateHITResponse
--
--         , responseGetAccountBalance $
--             newGetAccountBalanceResponse
--
--           ]
--     ]

-- Requests

requestApproveAssignment :: ApproveAssignment -> TestTree
requestApproveAssignment =
  req
    "ApproveAssignment"
    "fixture/ApproveAssignment.yaml"

requestListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHIT -> TestTree
requestListReviewPolicyResultsForHIT =
  req
    "ListReviewPolicyResultsForHIT"
    "fixture/ListReviewPolicyResultsForHIT.yaml"

requestListHITs :: ListHITs -> TestTree
requestListHITs =
  req
    "ListHITs"
    "fixture/ListHITs.yaml"

requestListWorkersWithQualificationType :: ListWorkersWithQualificationType -> TestTree
requestListWorkersWithQualificationType =
  req
    "ListWorkersWithQualificationType"
    "fixture/ListWorkersWithQualificationType.yaml"

requestDeleteHIT :: DeleteHIT -> TestTree
requestDeleteHIT =
  req
    "DeleteHIT"
    "fixture/DeleteHIT.yaml"

requestListReviewableHITs :: ListReviewableHITs -> TestTree
requestListReviewableHITs =
  req
    "ListReviewableHITs"
    "fixture/ListReviewableHITs.yaml"

requestGetAssignment :: GetAssignment -> TestTree
requestGetAssignment =
  req
    "GetAssignment"
    "fixture/GetAssignment.yaml"

requestDeleteQualificationType :: DeleteQualificationType -> TestTree
requestDeleteQualificationType =
  req
    "DeleteQualificationType"
    "fixture/DeleteQualificationType.yaml"

requestUpdateQualificationType :: UpdateQualificationType -> TestTree
requestUpdateQualificationType =
  req
    "UpdateQualificationType"
    "fixture/UpdateQualificationType.yaml"

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

requestNotifyWorkers :: NotifyWorkers -> TestTree
requestNotifyWorkers =
  req
    "NotifyWorkers"
    "fixture/NotifyWorkers.yaml"

requestCreateHITWithHITType :: CreateHITWithHITType -> TestTree
requestCreateHITWithHITType =
  req
    "CreateHITWithHITType"
    "fixture/CreateHITWithHITType.yaml"

requestCreateHITType :: CreateHITType -> TestTree
requestCreateHITType =
  req
    "CreateHITType"
    "fixture/CreateHITType.yaml"

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

requestUpdateExpirationForHIT :: UpdateExpirationForHIT -> TestTree
requestUpdateExpirationForHIT =
  req
    "UpdateExpirationForHIT"
    "fixture/UpdateExpirationForHIT.yaml"

requestRejectAssignment :: RejectAssignment -> TestTree
requestRejectAssignment =
  req
    "RejectAssignment"
    "fixture/RejectAssignment.yaml"

requestListAssignmentsForHIT :: ListAssignmentsForHIT -> TestTree
requestListAssignmentsForHIT =
  req
    "ListAssignmentsForHIT"
    "fixture/ListAssignmentsForHIT.yaml"

requestRejectQualificationRequest :: RejectQualificationRequest -> TestTree
requestRejectQualificationRequest =
  req
    "RejectQualificationRequest"
    "fixture/RejectQualificationRequest.yaml"

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

requestUpdateHITReviewStatus :: UpdateHITReviewStatus -> TestTree
requestUpdateHITReviewStatus =
  req
    "UpdateHITReviewStatus"
    "fixture/UpdateHITReviewStatus.yaml"

requestCreateQualificationType :: CreateQualificationType -> TestTree
requestCreateQualificationType =
  req
    "CreateQualificationType"
    "fixture/CreateQualificationType.yaml"

requestAcceptQualificationRequest :: AcceptQualificationRequest -> TestTree
requestAcceptQualificationRequest =
  req
    "AcceptQualificationRequest"
    "fixture/AcceptQualificationRequest.yaml"

requestGetFileUploadURL :: GetFileUploadURL -> TestTree
requestGetFileUploadURL =
  req
    "GetFileUploadURL"
    "fixture/GetFileUploadURL.yaml"

requestCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHIT -> TestTree
requestCreateAdditionalAssignmentsForHIT =
  req
    "CreateAdditionalAssignmentsForHIT"
    "fixture/CreateAdditionalAssignmentsForHIT.yaml"

requestGetHIT :: GetHIT -> TestTree
requestGetHIT =
  req
    "GetHIT"
    "fixture/GetHIT.yaml"

requestCreateWorkerBlock :: CreateWorkerBlock -> TestTree
requestCreateWorkerBlock =
  req
    "CreateWorkerBlock"
    "fixture/CreateWorkerBlock.yaml"

requestListHITsForQualificationType :: ListHITsForQualificationType -> TestTree
requestListHITsForQualificationType =
  req
    "ListHITsForQualificationType"
    "fixture/ListHITsForQualificationType.yaml"

requestListBonusPayments :: ListBonusPayments -> TestTree
requestListBonusPayments =
  req
    "ListBonusPayments"
    "fixture/ListBonusPayments.yaml"

requestListWorkerBlocks :: ListWorkerBlocks -> TestTree
requestListWorkerBlocks =
  req
    "ListWorkerBlocks"
    "fixture/ListWorkerBlocks.yaml"

requestDeleteWorkerBlock :: DeleteWorkerBlock -> TestTree
requestDeleteWorkerBlock =
  req
    "DeleteWorkerBlock"
    "fixture/DeleteWorkerBlock.yaml"

requestUpdateNotificationSettings :: UpdateNotificationSettings -> TestTree
requestUpdateNotificationSettings =
  req
    "UpdateNotificationSettings"
    "fixture/UpdateNotificationSettings.yaml"

requestAssociateQualificationWithWorker :: AssociateQualificationWithWorker -> TestTree
requestAssociateQualificationWithWorker =
  req
    "AssociateQualificationWithWorker"
    "fixture/AssociateQualificationWithWorker.yaml"

requestCreateHIT :: CreateHIT -> TestTree
requestCreateHIT =
  req
    "CreateHIT"
    "fixture/CreateHIT.yaml"

requestGetAccountBalance :: GetAccountBalance -> TestTree
requestGetAccountBalance =
  req
    "GetAccountBalance"
    "fixture/GetAccountBalance.yaml"

-- Responses

responseApproveAssignment :: ApproveAssignmentResponse -> TestTree
responseApproveAssignment =
  res
    "ApproveAssignmentResponse"
    "fixture/ApproveAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApproveAssignment)

responseListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHITResponse -> TestTree
responseListReviewPolicyResultsForHIT =
  res
    "ListReviewPolicyResultsForHITResponse"
    "fixture/ListReviewPolicyResultsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReviewPolicyResultsForHIT)

responseListHITs :: ListHITsResponse -> TestTree
responseListHITs =
  res
    "ListHITsResponse"
    "fixture/ListHITsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHITs)

responseListWorkersWithQualificationType :: ListWorkersWithQualificationTypeResponse -> TestTree
responseListWorkersWithQualificationType =
  res
    "ListWorkersWithQualificationTypeResponse"
    "fixture/ListWorkersWithQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkersWithQualificationType)

responseDeleteHIT :: DeleteHITResponse -> TestTree
responseDeleteHIT =
  res
    "DeleteHITResponse"
    "fixture/DeleteHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHIT)

responseListReviewableHITs :: ListReviewableHITsResponse -> TestTree
responseListReviewableHITs =
  res
    "ListReviewableHITsResponse"
    "fixture/ListReviewableHITsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReviewableHITs)

responseGetAssignment :: GetAssignmentResponse -> TestTree
responseGetAssignment =
  res
    "GetAssignmentResponse"
    "fixture/GetAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssignment)

responseDeleteQualificationType :: DeleteQualificationTypeResponse -> TestTree
responseDeleteQualificationType =
  res
    "DeleteQualificationTypeResponse"
    "fixture/DeleteQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQualificationType)

responseUpdateQualificationType :: UpdateQualificationTypeResponse -> TestTree
responseUpdateQualificationType =
  res
    "UpdateQualificationTypeResponse"
    "fixture/UpdateQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQualificationType)

responseListQualificationTypes :: ListQualificationTypesResponse -> TestTree
responseListQualificationTypes =
  res
    "ListQualificationTypesResponse"
    "fixture/ListQualificationTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQualificationTypes)

responseUpdateHITTypeOfHIT :: UpdateHITTypeOfHITResponse -> TestTree
responseUpdateHITTypeOfHIT =
  res
    "UpdateHITTypeOfHITResponse"
    "fixture/UpdateHITTypeOfHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHITTypeOfHIT)

responseDisassociateQualificationFromWorker :: DisassociateQualificationFromWorkerResponse -> TestTree
responseDisassociateQualificationFromWorker =
  res
    "DisassociateQualificationFromWorkerResponse"
    "fixture/DisassociateQualificationFromWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateQualificationFromWorker)

responseSendTestEventNotification :: SendTestEventNotificationResponse -> TestTree
responseSendTestEventNotification =
  res
    "SendTestEventNotificationResponse"
    "fixture/SendTestEventNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendTestEventNotification)

responseNotifyWorkers :: NotifyWorkersResponse -> TestTree
responseNotifyWorkers =
  res
    "NotifyWorkersResponse"
    "fixture/NotifyWorkersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyWorkers)

responseCreateHITWithHITType :: CreateHITWithHITTypeResponse -> TestTree
responseCreateHITWithHITType =
  res
    "CreateHITWithHITTypeResponse"
    "fixture/CreateHITWithHITTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHITWithHITType)

responseCreateHITType :: CreateHITTypeResponse -> TestTree
responseCreateHITType =
  res
    "CreateHITTypeResponse"
    "fixture/CreateHITTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHITType)

responseSendBonus :: SendBonusResponse -> TestTree
responseSendBonus =
  res
    "SendBonusResponse"
    "fixture/SendBonusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendBonus)

responseListQualificationRequests :: ListQualificationRequestsResponse -> TestTree
responseListQualificationRequests =
  res
    "ListQualificationRequestsResponse"
    "fixture/ListQualificationRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQualificationRequests)

responseUpdateExpirationForHIT :: UpdateExpirationForHITResponse -> TestTree
responseUpdateExpirationForHIT =
  res
    "UpdateExpirationForHITResponse"
    "fixture/UpdateExpirationForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExpirationForHIT)

responseRejectAssignment :: RejectAssignmentResponse -> TestTree
responseRejectAssignment =
  res
    "RejectAssignmentResponse"
    "fixture/RejectAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectAssignment)

responseListAssignmentsForHIT :: ListAssignmentsForHITResponse -> TestTree
responseListAssignmentsForHIT =
  res
    "ListAssignmentsForHITResponse"
    "fixture/ListAssignmentsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssignmentsForHIT)

responseRejectQualificationRequest :: RejectQualificationRequestResponse -> TestTree
responseRejectQualificationRequest =
  res
    "RejectQualificationRequestResponse"
    "fixture/RejectQualificationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectQualificationRequest)

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

responseUpdateHITReviewStatus :: UpdateHITReviewStatusResponse -> TestTree
responseUpdateHITReviewStatus =
  res
    "UpdateHITReviewStatusResponse"
    "fixture/UpdateHITReviewStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHITReviewStatus)

responseCreateQualificationType :: CreateQualificationTypeResponse -> TestTree
responseCreateQualificationType =
  res
    "CreateQualificationTypeResponse"
    "fixture/CreateQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQualificationType)

responseAcceptQualificationRequest :: AcceptQualificationRequestResponse -> TestTree
responseAcceptQualificationRequest =
  res
    "AcceptQualificationRequestResponse"
    "fixture/AcceptQualificationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptQualificationRequest)

responseGetFileUploadURL :: GetFileUploadURLResponse -> TestTree
responseGetFileUploadURL =
  res
    "GetFileUploadURLResponse"
    "fixture/GetFileUploadURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFileUploadURL)

responseCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHITResponse -> TestTree
responseCreateAdditionalAssignmentsForHIT =
  res
    "CreateAdditionalAssignmentsForHITResponse"
    "fixture/CreateAdditionalAssignmentsForHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAdditionalAssignmentsForHIT)

responseGetHIT :: GetHITResponse -> TestTree
responseGetHIT =
  res
    "GetHITResponse"
    "fixture/GetHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHIT)

responseCreateWorkerBlock :: CreateWorkerBlockResponse -> TestTree
responseCreateWorkerBlock =
  res
    "CreateWorkerBlockResponse"
    "fixture/CreateWorkerBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkerBlock)

responseListHITsForQualificationType :: ListHITsForQualificationTypeResponse -> TestTree
responseListHITsForQualificationType =
  res
    "ListHITsForQualificationTypeResponse"
    "fixture/ListHITsForQualificationTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHITsForQualificationType)

responseListBonusPayments :: ListBonusPaymentsResponse -> TestTree
responseListBonusPayments =
  res
    "ListBonusPaymentsResponse"
    "fixture/ListBonusPaymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBonusPayments)

responseListWorkerBlocks :: ListWorkerBlocksResponse -> TestTree
responseListWorkerBlocks =
  res
    "ListWorkerBlocksResponse"
    "fixture/ListWorkerBlocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkerBlocks)

responseDeleteWorkerBlock :: DeleteWorkerBlockResponse -> TestTree
responseDeleteWorkerBlock =
  res
    "DeleteWorkerBlockResponse"
    "fixture/DeleteWorkerBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkerBlock)

responseUpdateNotificationSettings :: UpdateNotificationSettingsResponse -> TestTree
responseUpdateNotificationSettings =
  res
    "UpdateNotificationSettingsResponse"
    "fixture/UpdateNotificationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotificationSettings)

responseAssociateQualificationWithWorker :: AssociateQualificationWithWorkerResponse -> TestTree
responseAssociateQualificationWithWorker =
  res
    "AssociateQualificationWithWorkerResponse"
    "fixture/AssociateQualificationWithWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateQualificationWithWorker)

responseCreateHIT :: CreateHITResponse -> TestTree
responseCreateHIT =
  res
    "CreateHITResponse"
    "fixture/CreateHITResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHIT)

responseGetAccountBalance :: GetAccountBalanceResponse -> TestTree
responseGetAccountBalance =
  res
    "GetAccountBalanceResponse"
    "fixture/GetAccountBalanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountBalance)
