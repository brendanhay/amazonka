{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.AWS.EC2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.EC2
    ( tests
    , fixtures
    ) where

import           Data.Time
import           Network.AWS.EC2
import           Network.AWS.Lens    ((&), (.~), (?~))
import           Network.AWS.Prelude
import           Test.AWS.Gen.EC2
import           Test.AWS.TH
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "request"
        [ requestDescribeVolumes $ describeVolumes
            & desVolumeIds .~ ["foo", "bar", "baz"]
            & desFilters   .~
                [ filter' "tag:Name" & fValues .~ ["octopus"]
                , filter' "tag:Role" & fValues .~ ["database", "server"]
                ]

        , requestDescribeImages $ describeImages
            & deseFilters .~
                [ filter' "is-public"    & fValues .~ ["true"]
                , filter' "architecture" & fValues .~ ["x86_64"]
                , filter' "platform"     & fValues .~ ["windows"]
                ]

        , requestDescribeInstances $ describeInstances
            & diiInstanceIds .~ ["i-foo", "i-bar", "i-baz"]
            & diiFilters     .~
                [ filter' "instance-type"
                    & fValues .~ ["m1.small", "m1.large"]
                , filter' "block-device-mapping.status"
                    & fValues .~ ["attached"]
                ]

        , requestCopySnapshot $ copySnapshot "us-west-1" "snap-1a2b3c4d"
            & csDescription ?~ "My_snapshot"

        ]

    , testGroup "response"
        [ responseDescribeInstances $ describeInstancesResponse 200
            & dirsReservations .~
                [ reservation "r-1a2b3c4d" "123456789012"
                    & rGroups .~
                        [ groupIdentifier
                            & giGroupId   ?~ "sg-1a2b3c4d"
                            & giGroupName ?~ "my-security-group"
                        ]

                    & rInstances .~
                        [ instance' "i-1a2b3c4d" "ami-1a2b3c4d" 0 C1_Medium
                            $(mkTime "2014-03-18T21:47:02+0000")
                            (placement & pAvailabilityZone ?~ "us-west-2a"
                                       & pTenancy          ?~ Default)
                            (monitoring & mState ?~ MSDisabled)
                            X86_64 EBS HVM Xen
                            (instanceState ISNRunning 16)
                                & insPlatform            ?~ Windows
                                & insClientToken         ?~ "ABCDE1234567890123"
                                & insSourceDestCheck     ?~ True
                                & insVPCId               ?~ "vpc-1a2b3c4d"
                                & insKeyName             ?~ "my-key-pair"
                                & insSubnetId            ?~ "subnet-1a2b3c4d"
                                & insRootDeviceName      ?~ "/dev/sda1"
                                & insPrivateIPAddress    ?~ "10.0.0.12"
                                & insPublicIPAddress     ?~ "46.51.219.63"
                                & insTags                .~ [tag "Name" "Windows Instance"]
                                & insState               .~ instanceState ISNRunning 16
                                & insSecurityGroups      .~
                                    [ groupIdentifier
                                        & giGroupId   ?~ "sg-1a2b3c4d"
                                        & giGroupName ?~ "my-security-group"
                                    ]

                                & insBlockDeviceMappings .~
                                    [ instanceBlockDeviceMapping
                                        & ibdmDeviceName ?~ "/dev/sda1"
                                        & ibdmEBS        ?~
                                            (ebsInstanceBlockDevice
                                                & eibdDeleteOnTermination ?~ True
                                                & eibdStatus              ?~ AAttached
                                                & eibdVolumeId            ?~ "vol-1a2b3c4d"
                                                & eibdAttachTime          ?~ $(mkTime "2014-03-18T21:47:02+0000"))
                                    ]

                                & insNetworkInterfaces   .~
                                    [ instanceNetworkInterface
                                        & iniStatus              ?~ NISInUse
                                        & iniSourceDestCheck     ?~ True
                                        & iniVPCId               ?~ "vpc-1a2b3c4d"
                                        & iniNetworkInterfaceId  ?~ "eni-1a2b3c4d"
                                        & iniSubnetId            ?~ "subnet-1a2b3c4d"
                                        & iniMACAddress          ?~ "1b:2b:3c:4d:5e:6f"
                                        & iniOwnerId             ?~ "123456789012"
                                        & iniPrivateIPAddress    ?~ "10.0.0.12"
                                        & iniDescription         ?~ "Primary network interface"
                                        & iniAttachment          ?~
                                            (instanceNetworkInterfaceAttachment
                                                & iniaDeleteOnTermination ?~ True
                                                & iniaStatus              ?~ AAttached
                                                & iniaAttachmentId        ?~ "eni-attach-1a2b3c4d"
                                                & iniaAttachTime          ?~ $(mkTime "2014-03-18T21:47:02+0000")
                                                & iniaDeviceIndex         ?~ 0)

                                        & iniAssociation         ?~
                                            (instanceNetworkInterfaceAssociation
                                                & iniaIPOwnerId  ?~ "123456789012"
                                                & iniaPublicIP   ?~ "198.51.100.63")

                                        & iniGroups              .~
                                            [ groupIdentifier
                                                & giGroupId   ?~ "sg-1a2b3c4d"
                                                & giGroupName ?~ "my-security-group"
                                            ]
                                    ]

                        ]
                ]
        ]
    ]

