{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.AWS.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay
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
import           Network.AWS.Prelude
import           Test.AWS.Gen.EC2
import           Test.AWS.TH
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ testDescribeInstancesResponse $ describeInstancesResponse 200
            & dirReservations .~
                [ reservation "r-1a2b3c4d" "123456789012"
                    & resGroups .~
                        [ groupIdentifier
                            & giGroupId   ?~ "sg-1a2b3c4d"
                            & giGroupName ?~ "my-security-group"
                        ]

                    & resInstances .~
                        [ instance'
                            "i-1a2b3c4d"
                            "ami-1a2b3c4d"
                            0
                            C1Medium
                            $(mkTime "2014-03-18T21:47:02+0000")
                            (placement & plaAvailabilityZone ?~ "us-east-1a"
                                       & plaTenancy          ?~ Default)
                            (monitoring & monState ?~ MSDisabled)
                            X8664
                            EBS
                            HVM
                            Xen
                            (instanceState ISNRunning 16)
                                & insPlatform            ?~ PVWindows
                                & insClientToken         ?~ "ABCDE1234567890123"
                                & insSourceDestCheck     ?~ True
                                & insVPCId               ?~ "vpc-1a2b3c4d"
                                & insKeyName             ?~ "my-key-pair"
                                & insSubnetId            ?~ "subnet-1a2b3c4d"
                                & insRootDeviceName      ?~ "/dev/sda1"
                                & insPrivateIPAddress    ?~ "10.0.0.12"
                                & insPublicIPAddress     ?~ "46.51.219.63"
                                & insTags                .~ [tag "Name" "Windows Instance"]
                                & insNetworkInterfaces   .~ []
                                & insBlockDeviceMappings .~ []
--                                    [InstanceBlockDeviceMapping' {ibdmEBS ?~ (EBSInstanceBlockDevice' {eibdDeleteOnTermination ?~ True

                        ]
                ]
        ]
    ]
-- insState                                        .~ instanceState' {isName .~ ISNRunning & isCode .~ 16}}]

 -- iniStatus              ?~ NISINUse
 -- iniSourceDestCheck     ?~ True
 -- iniVPCId               ?~ "vpc-1a2b3c4d"
 -- iniNetworkInterfaceId  ?~ "eni-1a2b3c4d"
 -- iniSubnetId            ?~ "subnet-1a2b3c4d"
 -- iniAttachment          ?~ (InstanceNetworkInterfaceAttachment' {iniaDeleteOnTermination ?~ True
 -- iniGroups              .~
 --     [ groupIdentifier & giGroupId ?~ "sg-1a2b3c4d" & giGroupName ?~ "my-security-group"
 --     ]

 -- ipiaPrivateIPAddress   ?~ "10.0.0.12"
 -- ipiaAssociation        ?~ (InstanceNetworkInterfaceAssociation' {iniaPublicDNSName = Nothing
 -- iniaIPOwnerId          ?~ "123456789012"
 -- iniaPublicIP           ?~ "198.51.100.63"})},InstancePrivateIPAddress' {ipiaPrimary ?~ False
 -- ipiaPrivateIPAddress   ?~ "10.0.0.14"
 -- ipiaAssociation        ?~ (InstanceNetworkInterfaceAssociation' {iniaPublicDNSName = Nothing
 -- iniaIPOwnerId          ?~ "123456789012"
 -- iniaPublicIP           ?~ "198.51.100.177"})}]
 -- iniaStatus             ?~ Attached
 -- iniaAttachmentId       ?~ "eni-attach-1a2b3c4d"
 -- iniaAttachTime         ?~ (Time 2014-03-18 21:47:02 UTC)
 -- iniaDeviceIndex        ?~ 0})
 -- iniMACAddress          ?~ "1b:2b:3c:4d:5e:6f"
 -- iniOwnerId             ?~ "123456789012"
 -- iniPrivateIPAddress    ?~ "10.0.0.12"
 -- iniDescription         ?~ "Primary network interface"
 -- iniAssociation         ?~ (InstanceNetworkInterfaceAssociation' {iniaPublicDNSName = Nothing
 -- iniaIPOwnerId          ?~ "123456789012"
 -- iniaPublicIP           ?~ "198.51.100.63"})}]
 -- eibdStatus             ?~ Attached
 -- eibdVolumeId           ?~ "vol-1a2b3c4d"
 -- eibdAttachTime         ?~ (Time 2014-05-19 10:48:12.345 UTC)})
 -- ibdmDeviceName         ?~ "/dev/sda1"}]
 -- plaTenancy             ?~ Default}

