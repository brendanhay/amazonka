{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.AWS.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.S3
    ( tests
    , fixtures
    ) where

import           Data.Time
import           Network.AWS.Prelude
import           Network.AWS.S3
import           Test.AWS.Gen.S3
import           Test.AWS.Prelude
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ testGetBucketReplicationResponse $
            getBucketReplicationResponse 200
                & gbrrReplicationConfiguration ?~
                   (replicationConfiguration "arn:aws:iam::35667example:role/CrossRegionReplicationRoleForS3"
                       & rcRules .~
                           [ replicationRule "" Enabled (destination "arn:aws:s3:::exampletargetbucket")
                               & rrId ?~ "rule1"
                           ])

        , testGetBucketPolicyResponse $
            getBucketPolicyResponse 200
                & gbprPolicy ?~ "foo"

        , testCopyObjectResponse $
            copyObjectResponse 200
                & corCopyObjectResult ?~
                    (copyObjectResult
                        & corETag         ?~ ETag "\"9b2cf535f27731c974343645a3985328\""
                        & corLastModified ?~ $(mkTime "2009-10-28T22:32:00Z"))
        ]
    ]
