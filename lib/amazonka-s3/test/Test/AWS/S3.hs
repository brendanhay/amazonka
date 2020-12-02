{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.AWS.S3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.S3
    ( tests
    , fixtures
    ) where

import Data.Time
import Network.AWS.Prelude
import Network.AWS.S3
import Test.AWS.Gen.S3
import Test.AWS.Prelude
import Test.AWS.S3.Internal

tests :: [TestTree]
tests =
    [ objectKeyTests
    ]

fixtures :: [TestTree]
fixtures =
    [ testGroup "request"
        [ requestDeleteObjects $
            deleteObjects "bucketname" $
                delete' & dObjects .~
                    [ objectIdentifier "sample1.text"
                    , objectIdentifier "sample2.text"
                    ]

        , requestListMultipartUploads $
            listMultipartUploads "foo-bucket" & lmuMaxUploads ?~ 3

        , requestPutObjectACLWithBody $
            putObjectACL "bucket-body" "key-body"
                & poaAccessControlPolicy ?~
                    ( accessControlPolicy
                        & acpGrants .~ [grant & gPermission ?~ PWrite]
                        & acpOwner  ?~ (owner & oId         ?~ "foo-oid")
                    )

        , requestPutObjectACLWithHeaders $
            putObjectACL "bucket-headers" "key-headers"
                & poaACL ?~ OBucketOwnerRead
        ]

    , testGroup "response"
        [ responseGetBucketReplication $
            getBucketReplicationResponse 200
                & gbrrsReplicationConfiguration ?~
                   (replicationConfiguration "arn:aws:iam::35667example:role/CrossRegionReplicationRoleForS3"
                       & rcRules .~
                           [ replicationRule "" Enabled (destination "arn:aws:s3:::exampletargetbucket")
                               & rrId ?~ "rule1"
                           ])

        , responseCopyObject $
            copyObjectResponse 200
                & corsCopyObjectResult ?~
                    (copyObjectResult
                        & corETag         ?~ ETag "\"9b2cf535f27731c974343645a3985328\""
                        & corLastModified ?~ $(mkTime "2009-10-28T22:32:00Z"))

        , responseListParts $
            listPartsResponse 200
                & lprsBucket               ?~ "example-bucket"
                & lprsKey                  ?~ "example-object"
                & lprsUploadId             ?~ "XXBsb2FkIElEIGZvciBlbHZpbmcncyVcdS1tb3ZpZS5tMnRzEEEwbG9hZA"
                & lprsStorageClass         ?~ Standard
                & lprsPartNumberMarker     ?~ 1
                & lprsNextPartNumberMarker ?~ 3
                & lprsMaxParts             ?~ 2
                & lprsIsTruncated          ?~ True
                & lprsInitiator            ?~
                    (initiator
                        & iId              ?~ "arn:aws:iam::111122223333:user/some-user-11116a31-17b5-4fb7-9df5-b288870f11xx"
                        & iDisplayName     ?~ "umat-user-11116a31-17b5-4fb7-9df5-b288870f11xx")
                & lprsOwner                ?~
                    (owner
                        & oId              ?~ "75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a"
                        & oDisplayName     ?~ "someName")
                & lprsParts                .~
                    [ part & pPartNumber   ?~ 2
                           & pLastModified ?~ $(mkTime "2010-11-10T20:48:34.000Z")
                           & pETag         ?~ "\"7778aef83f66abc1fa1e8477f296d394\""
                           & pSize         ?~ 10485760
                    , part & pPartNumber   ?~ 3
                           & pLastModified ?~ $(mkTime "2010-11-10T20:48:33.000Z")
                           & pETag         ?~ "\"aaaa18db4cc2f85cedef654fccc4a4x8\""
                           & pSize         ?~ 10485760
                    ]

        , responseGetBucketACL $
            getBucketACLResponse 200
                & gbarsOwner                  ?~
                    (owner
                      & oId                   ?~ "fedcba0987654321"
                      & oDisplayName          ?~ "foo")
                & gbarsGrants                 .~
                    [ grant
                        & gPermission         ?~ PFullControl
                        & gGrantee            ?~
                            (grantee CanonicalUser
                               & gId          ?~ "1234567890abcdef"
                               & gDisplayName ?~ "bar")
                    ]
        ]
    ]
