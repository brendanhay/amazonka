{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MechanicalTurk
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestApproveAssignment $
--             mkApproveAssignment
--
--         , requestListReviewPolicyResultsForHIT $
--             mkListReviewPolicyResultsForHIT
--
--         , requestListHITs $
--             mkListHITs
--
--         , requestListWorkersWithQualificationType $
--             mkListWorkersWithQualificationType
--
--         , requestDeleteHIT $
--             mkDeleteHIT
--
--         , requestListReviewableHITs $
--             mkListReviewableHITs
--
--         , requestGetAssignment $
--             mkGetAssignment
--
--         , requestDeleteQualificationType $
--             mkDeleteQualificationType
--
--         , requestUpdateQualificationType $
--             mkUpdateQualificationType
--
--         , requestListQualificationTypes $
--             mkListQualificationTypes
--
--         , requestUpdateHITTypeOfHIT $
--             mkUpdateHITTypeOfHIT
--
--         , requestDisassociateQualificationFromWorker $
--             mkDisassociateQualificationFromWorker
--
--         , requestSendTestEventNotification $
--             mkSendTestEventNotification
--
--         , requestNotifyWorkers $
--             mkNotifyWorkers
--
--         , requestCreateHITWithHITType $
--             mkCreateHITWithHITType
--
--         , requestCreateHITType $
--             mkCreateHITType
--
--         , requestSendBonus $
--             mkSendBonus
--
--         , requestListQualificationRequests $
--             mkListQualificationRequests
--
--         , requestUpdateExpirationForHIT $
--             mkUpdateExpirationForHIT
--
--         , requestRejectAssignment $
--             mkRejectAssignment
--
--         , requestListAssignmentsForHIT $
--             mkListAssignmentsForHIT
--
--         , requestRejectQualificationRequest $
--             mkRejectQualificationRequest
--
--         , requestGetQualificationScore $
--             mkGetQualificationScore
--
--         , requestGetQualificationType $
--             mkGetQualificationType
--
--         , requestUpdateHITReviewStatus $
--             mkUpdateHITReviewStatus
--
--         , requestCreateQualificationType $
--             mkCreateQualificationType
--
--         , requestAcceptQualificationRequest $
--             mkAcceptQualificationRequest
--
--         , requestGetFileUploadURL $
--             mkGetFileUploadURL
--
--         , requestCreateAdditionalAssignmentsForHIT $
--             mkCreateAdditionalAssignmentsForHIT
--
--         , requestGetHIT $
--             mkGetHIT
--
--         , requestCreateWorkerBlock $
--             mkCreateWorkerBlock
--
--         , requestListHITsForQualificationType $
--             mkListHITsForQualificationType
--
--         , requestListBonusPayments $
--             mkListBonusPayments
--
--         , requestListWorkerBlocks $
--             mkListWorkerBlocks
--
--         , requestDeleteWorkerBlock $
--             mkDeleteWorkerBlock
--
--         , requestUpdateNotificationSettings $
--             mkUpdateNotificationSettings
--
--         , requestAssociateQualificationWithWorker $
--             mkAssociateQualificationWithWorker
--
--         , requestCreateHIT $
--             mkCreateHIT
--
--         , requestGetAccountBalance $
--             mkGetAccountBalance
--
--           ]

--     , testGroup "response"
--         [ responseApproveAssignment $
--             mkApproveAssignmentResponse
--
--         , responseListReviewPolicyResultsForHIT $
--             mkListReviewPolicyResultsForHITResponse
--
--         , responseListHITs $
--             mkListHITsResponse
--
--         , responseListWorkersWithQualificationType $
--             mkListWorkersWithQualificationTypeResponse
--
--         , responseDeleteHIT $
--             mkDeleteHITResponse
--
--         , responseListReviewableHITs $
--             mkListReviewableHITsResponse
--
--         , responseGetAssignment $
--             mkGetAssignmentResponse
--
--         , responseDeleteQualificationType $
--             mkDeleteQualificationTypeResponse
--
--         , responseUpdateQualificationType $
--             mkUpdateQualificationTypeResponse
--
--         , responseListQualificationTypes $
--             mkListQualificationTypesResponse
--
--         , responseUpdateHITTypeOfHIT $
--             mkUpdateHITTypeOfHITResponse
--
--         , responseDisassociateQualificationFromWorker $
--             mkDisassociateQualificationFromWorkerResponse
--
--         , responseSendTestEventNotification $
--             mkSendTestEventNotificationResponse
--
--         , responseNotifyWorkers $
--             mkNotifyWorkersResponse
--
--         , responseCreateHITWithHITType $
--             mkCreateHITWithHITTypeResponse
--
--         , responseCreateHITType $
--             mkCreateHITTypeResponse
--
--         , responseSendBonus $
--             mkSendBonusResponse
--
--         , responseListQualificationRequests $
--             mkListQualificationRequestsResponse
--
--         , responseUpdateExpirationForHIT $
--             mkUpdateExpirationForHITResponse
--
--         , responseRejectAssignment $
--             mkRejectAssignmentResponse
--
--         , responseListAssignmentsForHIT $
--             mkListAssignmentsForHITResponse
--
--         , responseRejectQualificationRequest $
--             mkRejectQualificationRequestResponse
--
--         , responseGetQualificationScore $
--             mkGetQualificationScoreResponse
--
--         , responseGetQualificationType $
--             mkGetQualificationTypeResponse
--
--         , responseUpdateHITReviewStatus $
--             mkUpdateHITReviewStatusResponse
--
--         , responseCreateQualificationType $
--             mkCreateQualificationTypeResponse
--
--         , responseAcceptQualificationRequest $
--             mkAcceptQualificationRequestResponse
--
--         , responseGetFileUploadURL $
--             mkGetFileUploadURLResponse
--
--         , responseCreateAdditionalAssignmentsForHIT $
--             mkCreateAdditionalAssignmentsForHITResponse
--
--         , responseGetHIT $
--             mkGetHITResponse
--
--         , responseCreateWorkerBlock $
--             mkCreateWorkerBlockResponse
--
--         , responseListHITsForQualificationType $
--             mkListHITsForQualificationTypeResponse
--
--         , responseListBonusPayments $
--             mkListBonusPaymentsResponse
--
--         , responseListWorkerBlocks $
--             mkListWorkerBlocksResponse
--
--         , responseDeleteWorkerBlock $
--             mkDeleteWorkerBlockResponse
--
--         , responseUpdateNotificationSettings $
--             mkUpdateNotificationSettingsResponse
--
--         , responseAssociateQualificationWithWorker $
--             mkAssociateQualificationWithWorkerResponse
--
--         , responseCreateHIT $
--             mkCreateHITResponse
--
--         , responseGetAccountBalance $
--             mkGetAccountBalanceResponse
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
    mechanicalTurkService
    (Proxy :: Proxy ApproveAssignment)

responseListReviewPolicyResultsForHIT :: ListReviewPolicyResultsForHITResponse -> TestTree
responseListReviewPolicyResultsForHIT =
  res
    "ListReviewPolicyResultsForHITResponse"
    "fixture/ListReviewPolicyResultsForHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListReviewPolicyResultsForHIT)

responseListHITs :: ListHITsResponse -> TestTree
responseListHITs =
  res
    "ListHITsResponse"
    "fixture/ListHITsResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListHITs)

responseListWorkersWithQualificationType :: ListWorkersWithQualificationTypeResponse -> TestTree
responseListWorkersWithQualificationType =
  res
    "ListWorkersWithQualificationTypeResponse"
    "fixture/ListWorkersWithQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListWorkersWithQualificationType)

responseDeleteHIT :: DeleteHITResponse -> TestTree
responseDeleteHIT =
  res
    "DeleteHITResponse"
    "fixture/DeleteHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy DeleteHIT)

responseListReviewableHITs :: ListReviewableHITsResponse -> TestTree
responseListReviewableHITs =
  res
    "ListReviewableHITsResponse"
    "fixture/ListReviewableHITsResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListReviewableHITs)

responseGetAssignment :: GetAssignmentResponse -> TestTree
responseGetAssignment =
  res
    "GetAssignmentResponse"
    "fixture/GetAssignmentResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetAssignment)

responseDeleteQualificationType :: DeleteQualificationTypeResponse -> TestTree
responseDeleteQualificationType =
  res
    "DeleteQualificationTypeResponse"
    "fixture/DeleteQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy DeleteQualificationType)

responseUpdateQualificationType :: UpdateQualificationTypeResponse -> TestTree
responseUpdateQualificationType =
  res
    "UpdateQualificationTypeResponse"
    "fixture/UpdateQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy UpdateQualificationType)

responseListQualificationTypes :: ListQualificationTypesResponse -> TestTree
responseListQualificationTypes =
  res
    "ListQualificationTypesResponse"
    "fixture/ListQualificationTypesResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListQualificationTypes)

responseUpdateHITTypeOfHIT :: UpdateHITTypeOfHITResponse -> TestTree
responseUpdateHITTypeOfHIT =
  res
    "UpdateHITTypeOfHITResponse"
    "fixture/UpdateHITTypeOfHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy UpdateHITTypeOfHIT)

responseDisassociateQualificationFromWorker :: DisassociateQualificationFromWorkerResponse -> TestTree
responseDisassociateQualificationFromWorker =
  res
    "DisassociateQualificationFromWorkerResponse"
    "fixture/DisassociateQualificationFromWorkerResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy DisassociateQualificationFromWorker)

responseSendTestEventNotification :: SendTestEventNotificationResponse -> TestTree
responseSendTestEventNotification =
  res
    "SendTestEventNotificationResponse"
    "fixture/SendTestEventNotificationResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy SendTestEventNotification)

responseNotifyWorkers :: NotifyWorkersResponse -> TestTree
responseNotifyWorkers =
  res
    "NotifyWorkersResponse"
    "fixture/NotifyWorkersResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy NotifyWorkers)

responseCreateHITWithHITType :: CreateHITWithHITTypeResponse -> TestTree
responseCreateHITWithHITType =
  res
    "CreateHITWithHITTypeResponse"
    "fixture/CreateHITWithHITTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateHITWithHITType)

responseCreateHITType :: CreateHITTypeResponse -> TestTree
responseCreateHITType =
  res
    "CreateHITTypeResponse"
    "fixture/CreateHITTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateHITType)

responseSendBonus :: SendBonusResponse -> TestTree
responseSendBonus =
  res
    "SendBonusResponse"
    "fixture/SendBonusResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy SendBonus)

responseListQualificationRequests :: ListQualificationRequestsResponse -> TestTree
responseListQualificationRequests =
  res
    "ListQualificationRequestsResponse"
    "fixture/ListQualificationRequestsResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListQualificationRequests)

responseUpdateExpirationForHIT :: UpdateExpirationForHITResponse -> TestTree
responseUpdateExpirationForHIT =
  res
    "UpdateExpirationForHITResponse"
    "fixture/UpdateExpirationForHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy UpdateExpirationForHIT)

responseRejectAssignment :: RejectAssignmentResponse -> TestTree
responseRejectAssignment =
  res
    "RejectAssignmentResponse"
    "fixture/RejectAssignmentResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy RejectAssignment)

responseListAssignmentsForHIT :: ListAssignmentsForHITResponse -> TestTree
responseListAssignmentsForHIT =
  res
    "ListAssignmentsForHITResponse"
    "fixture/ListAssignmentsForHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListAssignmentsForHIT)

responseRejectQualificationRequest :: RejectQualificationRequestResponse -> TestTree
responseRejectQualificationRequest =
  res
    "RejectQualificationRequestResponse"
    "fixture/RejectQualificationRequestResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy RejectQualificationRequest)

responseGetQualificationScore :: GetQualificationScoreResponse -> TestTree
responseGetQualificationScore =
  res
    "GetQualificationScoreResponse"
    "fixture/GetQualificationScoreResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetQualificationScore)

responseGetQualificationType :: GetQualificationTypeResponse -> TestTree
responseGetQualificationType =
  res
    "GetQualificationTypeResponse"
    "fixture/GetQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetQualificationType)

responseUpdateHITReviewStatus :: UpdateHITReviewStatusResponse -> TestTree
responseUpdateHITReviewStatus =
  res
    "UpdateHITReviewStatusResponse"
    "fixture/UpdateHITReviewStatusResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy UpdateHITReviewStatus)

responseCreateQualificationType :: CreateQualificationTypeResponse -> TestTree
responseCreateQualificationType =
  res
    "CreateQualificationTypeResponse"
    "fixture/CreateQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateQualificationType)

responseAcceptQualificationRequest :: AcceptQualificationRequestResponse -> TestTree
responseAcceptQualificationRequest =
  res
    "AcceptQualificationRequestResponse"
    "fixture/AcceptQualificationRequestResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy AcceptQualificationRequest)

responseGetFileUploadURL :: GetFileUploadURLResponse -> TestTree
responseGetFileUploadURL =
  res
    "GetFileUploadURLResponse"
    "fixture/GetFileUploadURLResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetFileUploadURL)

responseCreateAdditionalAssignmentsForHIT :: CreateAdditionalAssignmentsForHITResponse -> TestTree
responseCreateAdditionalAssignmentsForHIT =
  res
    "CreateAdditionalAssignmentsForHITResponse"
    "fixture/CreateAdditionalAssignmentsForHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateAdditionalAssignmentsForHIT)

responseGetHIT :: GetHITResponse -> TestTree
responseGetHIT =
  res
    "GetHITResponse"
    "fixture/GetHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetHIT)

responseCreateWorkerBlock :: CreateWorkerBlockResponse -> TestTree
responseCreateWorkerBlock =
  res
    "CreateWorkerBlockResponse"
    "fixture/CreateWorkerBlockResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateWorkerBlock)

responseListHITsForQualificationType :: ListHITsForQualificationTypeResponse -> TestTree
responseListHITsForQualificationType =
  res
    "ListHITsForQualificationTypeResponse"
    "fixture/ListHITsForQualificationTypeResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListHITsForQualificationType)

responseListBonusPayments :: ListBonusPaymentsResponse -> TestTree
responseListBonusPayments =
  res
    "ListBonusPaymentsResponse"
    "fixture/ListBonusPaymentsResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListBonusPayments)

responseListWorkerBlocks :: ListWorkerBlocksResponse -> TestTree
responseListWorkerBlocks =
  res
    "ListWorkerBlocksResponse"
    "fixture/ListWorkerBlocksResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy ListWorkerBlocks)

responseDeleteWorkerBlock :: DeleteWorkerBlockResponse -> TestTree
responseDeleteWorkerBlock =
  res
    "DeleteWorkerBlockResponse"
    "fixture/DeleteWorkerBlockResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy DeleteWorkerBlock)

responseUpdateNotificationSettings :: UpdateNotificationSettingsResponse -> TestTree
responseUpdateNotificationSettings =
  res
    "UpdateNotificationSettingsResponse"
    "fixture/UpdateNotificationSettingsResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy UpdateNotificationSettings)

responseAssociateQualificationWithWorker :: AssociateQualificationWithWorkerResponse -> TestTree
responseAssociateQualificationWithWorker =
  res
    "AssociateQualificationWithWorkerResponse"
    "fixture/AssociateQualificationWithWorkerResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy AssociateQualificationWithWorker)

responseCreateHIT :: CreateHITResponse -> TestTree
responseCreateHIT =
  res
    "CreateHITResponse"
    "fixture/CreateHITResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy CreateHIT)

responseGetAccountBalance :: GetAccountBalanceResponse -> TestTree
responseGetAccountBalance =
  res
    "GetAccountBalanceResponse"
    "fixture/GetAccountBalanceResponse.proto"
    mechanicalTurkService
    (Proxy :: Proxy GetAccountBalance)
