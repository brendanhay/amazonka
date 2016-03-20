{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CertificateManager
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CertificateManager where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CertificateManager
import Test.AWS.CertificateManager.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testResendValidationEmail $
--             resendValidationEmail
--
--         , testGetCertificate $
--             getCertificate
--
--         , testRequestCertificate $
--             requestCertificate
--
--         , testListCertificates $
--             listCertificates
--
--         , testDeleteCertificate $
--             deleteCertificate
--
--         , testDescribeCertificate $
--             describeCertificate
--
--           ]

--     , testGroup "response"
--         [ testResendValidationEmailResponse $
--             resendValidationEmailResponse
--
--         , testGetCertificateResponse $
--             getCertificateResponse
--
--         , testRequestCertificateResponse $
--             requestCertificateResponse
--
--         , testListCertificatesResponse $
--             listCertificatesResponse
--
--         , testDeleteCertificateResponse $
--             deleteCertificateResponse
--
--         , testDescribeCertificateResponse $
--             describeCertificateResponse
--
--           ]
--     ]

-- Requests

testResendValidationEmail :: ResendValidationEmail -> TestTree
testResendValidationEmail = req
    "ResendValidationEmail"
    "fixture/ResendValidationEmail.yaml"

testGetCertificate :: GetCertificate -> TestTree
testGetCertificate = req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

testRequestCertificate :: RequestCertificate -> TestTree
testRequestCertificate = req
    "RequestCertificate"
    "fixture/RequestCertificate.yaml"

testListCertificates :: ListCertificates -> TestTree
testListCertificates = req
    "ListCertificates"
    "fixture/ListCertificates.yaml"

testDeleteCertificate :: DeleteCertificate -> TestTree
testDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

testDescribeCertificate :: DescribeCertificate -> TestTree
testDescribeCertificate = req
    "DescribeCertificate"
    "fixture/DescribeCertificate.yaml"

-- Responses

testResendValidationEmailResponse :: ResendValidationEmailResponse -> TestTree
testResendValidationEmailResponse = res
    "ResendValidationEmailResponse"
    "fixture/ResendValidationEmailResponse.proto"
    certificateManager
    (Proxy :: Proxy ResendValidationEmail)

testGetCertificateResponse :: GetCertificateResponse -> TestTree
testGetCertificateResponse = res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy GetCertificate)

testRequestCertificateResponse :: RequestCertificateResponse -> TestTree
testRequestCertificateResponse = res
    "RequestCertificateResponse"
    "fixture/RequestCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy RequestCertificate)

testListCertificatesResponse :: ListCertificatesResponse -> TestTree
testListCertificatesResponse = res
    "ListCertificatesResponse"
    "fixture/ListCertificatesResponse.proto"
    certificateManager
    (Proxy :: Proxy ListCertificates)

testDeleteCertificateResponse :: DeleteCertificateResponse -> TestTree
testDeleteCertificateResponse = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy DeleteCertificate)

testDescribeCertificateResponse :: DescribeCertificateResponse -> TestTree
testDescribeCertificateResponse = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy DescribeCertificate)
