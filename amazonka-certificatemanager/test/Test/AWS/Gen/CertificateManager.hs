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
--         , testListTagsForCertificate $
--             listTagsForCertificate
--
--         , testGetCertificate $
--             getCertificate
--
--         , testAddTagsToCertificate $
--             addTagsToCertificate
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
--         , testRemoveTagsFromCertificate $
--             removeTagsFromCertificate
--
--         , testDescribeCertificate $
--             describeCertificate
--
--           ]

--     , testGroup "response"
--         [ testResendValidationEmailResponse $
--             resendValidationEmailResponse
--
--         , testListTagsForCertificateResponse $
--             listTagsForCertificateResponse
--
--         , testGetCertificateResponse $
--             getCertificateResponse
--
--         , testAddTagsToCertificateResponse $
--             addTagsToCertificateResponse
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
--         , testRemoveTagsFromCertificateResponse $
--             removeTagsFromCertificateResponse
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

testListTagsForCertificate :: ListTagsForCertificate -> TestTree
testListTagsForCertificate = req
    "ListTagsForCertificate"
    "fixture/ListTagsForCertificate.yaml"

testGetCertificate :: GetCertificate -> TestTree
testGetCertificate = req
    "GetCertificate"
    "fixture/GetCertificate.yaml"

testAddTagsToCertificate :: AddTagsToCertificate -> TestTree
testAddTagsToCertificate = req
    "AddTagsToCertificate"
    "fixture/AddTagsToCertificate.yaml"

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

testRemoveTagsFromCertificate :: RemoveTagsFromCertificate -> TestTree
testRemoveTagsFromCertificate = req
    "RemoveTagsFromCertificate"
    "fixture/RemoveTagsFromCertificate.yaml"

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

testListTagsForCertificateResponse :: ListTagsForCertificateResponse -> TestTree
testListTagsForCertificateResponse = res
    "ListTagsForCertificateResponse"
    "fixture/ListTagsForCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy ListTagsForCertificate)

testGetCertificateResponse :: GetCertificateResponse -> TestTree
testGetCertificateResponse = res
    "GetCertificateResponse"
    "fixture/GetCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy GetCertificate)

testAddTagsToCertificateResponse :: AddTagsToCertificateResponse -> TestTree
testAddTagsToCertificateResponse = res
    "AddTagsToCertificateResponse"
    "fixture/AddTagsToCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy AddTagsToCertificate)

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

testRemoveTagsFromCertificateResponse :: RemoveTagsFromCertificateResponse -> TestTree
testRemoveTagsFromCertificateResponse = res
    "RemoveTagsFromCertificateResponse"
    "fixture/RemoveTagsFromCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy RemoveTagsFromCertificate)

testDescribeCertificateResponse :: DescribeCertificateResponse -> TestTree
testDescribeCertificateResponse = res
    "DescribeCertificateResponse"
    "fixture/DescribeCertificateResponse.proto"
    certificateManager
    (Proxy :: Proxy DescribeCertificate)
