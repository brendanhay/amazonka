{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PaymentCryptographyData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PaymentCryptographyData where

import Amazonka.PaymentCryptographyData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PaymentCryptographyData.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDecryptData $
--             newDecryptData
--
--         , requestEncryptData $
--             newEncryptData
--
--         , requestGenerateCardValidationData $
--             newGenerateCardValidationData
--
--         , requestGenerateMac $
--             newGenerateMac
--
--         , requestGeneratePinData $
--             newGeneratePinData
--
--         , requestReEncryptData $
--             newReEncryptData
--
--         , requestTranslatePinData $
--             newTranslatePinData
--
--         , requestVerifyAuthRequestCryptogram $
--             newVerifyAuthRequestCryptogram
--
--         , requestVerifyCardValidationData $
--             newVerifyCardValidationData
--
--         , requestVerifyMac $
--             newVerifyMac
--
--         , requestVerifyPinData $
--             newVerifyPinData
--
--           ]

--     , testGroup "response"
--         [ responseDecryptData $
--             newDecryptDataResponse
--
--         , responseEncryptData $
--             newEncryptDataResponse
--
--         , responseGenerateCardValidationData $
--             newGenerateCardValidationDataResponse
--
--         , responseGenerateMac $
--             newGenerateMacResponse
--
--         , responseGeneratePinData $
--             newGeneratePinDataResponse
--
--         , responseReEncryptData $
--             newReEncryptDataResponse
--
--         , responseTranslatePinData $
--             newTranslatePinDataResponse
--
--         , responseVerifyAuthRequestCryptogram $
--             newVerifyAuthRequestCryptogramResponse
--
--         , responseVerifyCardValidationData $
--             newVerifyCardValidationDataResponse
--
--         , responseVerifyMac $
--             newVerifyMacResponse
--
--         , responseVerifyPinData $
--             newVerifyPinDataResponse
--
--           ]
--     ]

-- Requests

requestDecryptData :: DecryptData -> TestTree
requestDecryptData =
  req
    "DecryptData"
    "fixture/DecryptData.yaml"

requestEncryptData :: EncryptData -> TestTree
requestEncryptData =
  req
    "EncryptData"
    "fixture/EncryptData.yaml"

requestGenerateCardValidationData :: GenerateCardValidationData -> TestTree
requestGenerateCardValidationData =
  req
    "GenerateCardValidationData"
    "fixture/GenerateCardValidationData.yaml"

requestGenerateMac :: GenerateMac -> TestTree
requestGenerateMac =
  req
    "GenerateMac"
    "fixture/GenerateMac.yaml"

requestGeneratePinData :: GeneratePinData -> TestTree
requestGeneratePinData =
  req
    "GeneratePinData"
    "fixture/GeneratePinData.yaml"

requestReEncryptData :: ReEncryptData -> TestTree
requestReEncryptData =
  req
    "ReEncryptData"
    "fixture/ReEncryptData.yaml"

requestTranslatePinData :: TranslatePinData -> TestTree
requestTranslatePinData =
  req
    "TranslatePinData"
    "fixture/TranslatePinData.yaml"

requestVerifyAuthRequestCryptogram :: VerifyAuthRequestCryptogram -> TestTree
requestVerifyAuthRequestCryptogram =
  req
    "VerifyAuthRequestCryptogram"
    "fixture/VerifyAuthRequestCryptogram.yaml"

requestVerifyCardValidationData :: VerifyCardValidationData -> TestTree
requestVerifyCardValidationData =
  req
    "VerifyCardValidationData"
    "fixture/VerifyCardValidationData.yaml"

requestVerifyMac :: VerifyMac -> TestTree
requestVerifyMac =
  req
    "VerifyMac"
    "fixture/VerifyMac.yaml"

requestVerifyPinData :: VerifyPinData -> TestTree
requestVerifyPinData =
  req
    "VerifyPinData"
    "fixture/VerifyPinData.yaml"

-- Responses

responseDecryptData :: DecryptDataResponse -> TestTree
responseDecryptData =
  res
    "DecryptDataResponse"
    "fixture/DecryptDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecryptData)

responseEncryptData :: EncryptDataResponse -> TestTree
responseEncryptData =
  res
    "EncryptDataResponse"
    "fixture/EncryptDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EncryptData)

responseGenerateCardValidationData :: GenerateCardValidationDataResponse -> TestTree
responseGenerateCardValidationData =
  res
    "GenerateCardValidationDataResponse"
    "fixture/GenerateCardValidationDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateCardValidationData)

responseGenerateMac :: GenerateMacResponse -> TestTree
responseGenerateMac =
  res
    "GenerateMacResponse"
    "fixture/GenerateMacResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateMac)

responseGeneratePinData :: GeneratePinDataResponse -> TestTree
responseGeneratePinData =
  res
    "GeneratePinDataResponse"
    "fixture/GeneratePinDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GeneratePinData)

responseReEncryptData :: ReEncryptDataResponse -> TestTree
responseReEncryptData =
  res
    "ReEncryptDataResponse"
    "fixture/ReEncryptDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReEncryptData)

responseTranslatePinData :: TranslatePinDataResponse -> TestTree
responseTranslatePinData =
  res
    "TranslatePinDataResponse"
    "fixture/TranslatePinDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TranslatePinData)

responseVerifyAuthRequestCryptogram :: VerifyAuthRequestCryptogramResponse -> TestTree
responseVerifyAuthRequestCryptogram =
  res
    "VerifyAuthRequestCryptogramResponse"
    "fixture/VerifyAuthRequestCryptogramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyAuthRequestCryptogram)

responseVerifyCardValidationData :: VerifyCardValidationDataResponse -> TestTree
responseVerifyCardValidationData =
  res
    "VerifyCardValidationDataResponse"
    "fixture/VerifyCardValidationDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyCardValidationData)

responseVerifyMac :: VerifyMacResponse -> TestTree
responseVerifyMac =
  res
    "VerifyMacResponse"
    "fixture/VerifyMacResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyMac)

responseVerifyPinData :: VerifyPinDataResponse -> TestTree
responseVerifyPinData =
  res
    "VerifyPinDataResponse"
    "fixture/VerifyPinDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyPinData)
