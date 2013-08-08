{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Time
import           Network.AWS.Internal

newtype CallerRef = CallerRef Text
    deriving (Show, IsString)

instance ToJSON CallerRef where
    toJSON (CallerRef s) = toJSON s

instance FromJSON CallerRef where
    parseJSON (String s) = return $ CallerRef s
    parseJSON _          = mzero

callerRef :: IO CallerRef
callerRef = fromString . show <$> getCurrentTime

data Protocol = HTTP | TCP
    deriving (Show)

$(deriveJSON defaultOptions ''Protocol)

data RecordAction = CreateAction | DeleteAction

instance Show RecordAction where
    show CreateAction = "CREATE"
    show DeleteAction = "DELETE"

instance ToJSON RecordAction where
    toJSON = String . Text.pack . show

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Show)

$(deriveJSON defaultOptions ''RecordType)

instance QueryParam RecordType where
    queryParam k v = [(k, BS.pack $ show v)]

data ChangeStatus = PENDING | INSYNC
    deriving (Show)

$(deriveJSON defaultOptions ''ChangeStatus)

data Config = Config
    { cComment :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''Config)

data HostedZone = HostedZone
    { hzId                     :: !Text
    , hzName                   :: !Text
    , hzCallerRef              :: !CallerRef
    , hzConfig                 :: !Config
    , hzResourceRecordSetCount :: !Integer
    } deriving (Show)

$(deriveJSON fieldOptions ''HostedZone)

data ChangeInfo = ChangeInfo
    { ciId          :: !Text
    , ciStatus      :: !ChangeStatus
    , ciSubmittedAt :: !UTCTime
    } deriving (Show)

$(deriveJSON fieldOptions ''ChangeInfo)

data DelegationSet = DelegationSet
    { dsNameServers :: ![Text]
    } deriving (Show)

$(deriveJSON fieldOptions ''DelegationSet)

--
-- Hosted Zones
--

-- <CreateHostedZoneResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HostedZone>
--       <Id>/hostedzone/Route 53 hosted zone ID</Id>
--       <Name>DNS domain name</Name>
--       <CallerReference>unique description</CallerReference>
--       <Config>
--          <Comment>optional comment</Comment>
--       </Config>
--       <ResourceRecordSetCount>number of resource record sets
--          in the hosted zone</ResourceRecordSetCount>
--    </HostedZone>
--    <ChangeInfo>
--       <Id>/change/unique identifier for the
--                              change batch request</Id>
--       <Status>PENDING | INSYNC</Status>
--       <SubmittedAt>date and time in Coordinated Universal Time
--          format</SubmittedAt>
--    </ChangeInfo>
--    <DelegationSet>
--       <NameServers>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--       </NameServers>
--    </DelegationSet>
-- </CreateHostedZoneResponse>

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { chzrHostedZone    :: !HostedZone
    , chzrChangeInfo    :: !ChangeInfo
    , chzrDelegationSet :: !DelegationSet
    } deriving (Show)

$(deriveJSON fieldOptions ''CreateHostedZoneResponse)

-- <GetHostedZoneResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HostedZone>
--       <Id>/hostedzone/Route 53 hosted zone ID</Id>
--       <Name>DNS domain name</Name>
--       <CallerReference>unique identifier that you specified
--          when you created the hosted zone</CallerReference>
--       <Config>
--          <Comment>comment that you specified when you
--             created the hosted zone</Comment>
--       </Config>
--       <ResourceRecordSetCount>number of resource record sets
--          in the hosted zone</ResourceRecordSetCount>
--    </HostedZone>
--    <DelegationSet>
--       <NameServers>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--          <NameServer>DNS name for Route 53 name server</NameServer>
--       </NameServers>
--    </DelegationSet>
-- </GetHostedZoneResponse>

data GetHostedZoneResponse = GetHostedZoneResponse
    { ghzrHostZone      :: !HostedZone
    , ghzrDelegationSet :: !DelegationSet
    } deriving (Show)

$(deriveJSON fieldOptions ''GetHostedZoneResponse)

-- <ListHostedZonesResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HostedZones>
--       <HostedZone>
--          <Id>/hostedzone/Route 53 hosted zone ID</Id>
--          <Name>DNS domain name</Name>
--          <CallerReference>unique description that you specified
--             when you created the hosted zone</CallerReference>
--          <Config>
--             <Comment>comment that you specified when you
--                created the hosted zone</Comment>
--          </Config>
--          <ResourceRecordSetCount>number of resource record sets
--             in the hosted zone</ResourceRecordSetCount>
--       </HostedZone>
--       ...
--    </HostedZones>
--    <Marker>value of the marker parameter,
--       if any, in the previous request</Marker>
--    <IsTruncated>true | false</IsTruncated>
--    <NextMarker>
--       if IsTruncated is true,
--       the hosted zone ID of the first hosted zone
--       in the next group of maxitems hosted zones
--    </NextMarker>
--    <MaxItems>value of the maxitems parameter,
--       if any, in the previous request</MaxItems>
-- </ListHostedZonesResponse>

data ListHostedZonesResponse = ListHostedZonesResponse
    { lhzrHostedZones :: ![HostedZone]
    , lhzrIsTruncated :: !Bool
    , lhzrMarker      :: !Text
    , lhzrNextMarker  :: !(Maybe Text)
    , lhzrMaxItems    :: !Integer
    } deriving (Show)

$(deriveJSON fieldOptions ''ListHostedZonesResponse)

-- <DeleteHostedZoneResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <ChangeInfo>
--       <Id>/change/C1PA6795UKMFR9</Id>
--       <Status>PENDING</Status>
--       <SubmittedAt>2012-03-10T01:36:41.958Z</SubmittedAt>
--    </ChangeInfo>
-- </DeleteHostedZoneResponse>

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { dhzrChangeInfo :: !ChangeInfo
    } deriving (Show)

$(deriveJSON fieldOptions ''DeleteHostedZoneResponse)

--
-- Record Sets
--

-- <ChangeResourceRecordSetsResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <ChangeInfo>
--       <Id>/change/C2682N5HXP0BZ4</Id>
--       <Status>PENDING</Status>
--       <SubmittedAt>2010-09-10T01:36:41.958Z</SubmittedAt>
--    </ChangeInfo>
-- </ChangeResourceRecordSetsResponse>

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { crrsrChangeInfo :: !ChangeInfo
    } deriving (Show)

$(deriveJSON fieldOptions ''ChangeResourceRecordSetsResponse)

-- <ListResourceRecordSetsResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <ResourceRecordSets>

--       <!-- Basic syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <TTL>time to live in seconds</TTL>
--          <ResourceRecords>
--             <ResourceRecord>
--                <Value>applicable value for the DNS record type</Value>
--             </ResourceRecord>
--          </ResourceRecords>
--          <HealthCheckId>ID of a Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Weighted resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--                   resource record set</SetIdentifier>
--          <Weight>value between 0 and 255</Weight>
--          <TTL>time to live in seconds</TTL>
--          <ResourceRecords>
--             <ResourceRecord>
--                <Value>applicable value for the DNS record type</Value>
--             </ResourceRecord>
--          </ResourceRecords>
--          <HealthCheckId>ID of a Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Alias resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <AliasTarget>
--             <HostedZoneId>hosted zone ID for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or Route 53 hosted zone</HostedZoneId>
--             <DNSName>DNS domain name for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or another resource record set
--                in this hosted zone</DNSName>
--             <EvaluateTargetHealth>true | false<EvaluateTargetHealth>
--          </AliasTarget>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Weighted alias resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--             resource record set</SetIdentifier>
--          <Weight>value between 0 and 255</Weight>
--          <AliasTarget>
--             <HostedZoneId>hosted zone ID for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or Route 53 hosted zone</HostedZoneId>
--             <DNSName>DNS domain name for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or another resource record set
--                in this hosted zone</DNSName>
--             <EvaluateTargetHealth>true | false<EvaluateTargetHealth>
--          </AliasTarget>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Latency resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--             resource record set</SetIdentifier>
--          <Region>Amazon EC2 region name</Region>
--          <TTL>time to live in seconds</TTL>
--          <ResourceRecords>
--             <ResourceRecord>
--                <Value>applicable value for the record type</Value>
--             </ResourceRecord>
--          </ResourceRecords>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Latency alias resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--             resource record set</SetIdentifier>
--          <Region>Amazon EC2 region name</Region>
--          <AliasTarget>
--             <HostedZoneId>hosted zone ID for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or Route 53 hosted zone</HostedZoneId>
--             <DNSName>DNS domain name for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or another resource record set
--                in this hosted zone</DNSName>
--             <EvaluateTargetHealth>true | false<EvaluateTargetHealth>
--          </AliasTarget>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Failover resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--             resource record set</SetIdentifier>
--          <Failover>PRIMARY | SECONDARY</Failover>
--          <TTL>time to live in seconds</TTL>
--          <ResourceRecords>
--             <ResourceRecord>
--                <Value>applicable value for the record type</Value>
--             </ResourceRecord>
--          </ResourceRecords>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>

--       <!-- Failover alias resource record set syntax -->
--       <ResourceRecordSet>
--          <Name>DNS domain name</Name>
--          <Type>DNS record type</Type>
--          <SetIdentifier>unique description for this
--             resource record set</SetIdentifier>
--          <Failover>PRIMARY | SECONDARY</Failover>
--          <AliasTarget>
--             <HostedZoneId>hosted zone ID for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or Route 53 hosted zone</HostedZoneId>
--             <DNSName>DNS domain name for your
--                CloudFront distribution, Amazon S3 bucket,
--                Elastic Load Balancing load balancer,
--                or another resource record set
--                in this hosted zone</DNSName>
--             <EvaluateTargetHealth>true | false</EvaluateTargetHealth>
--          </AliasTarget>
--          <HealthCheckId>optional ID of a
--             Route 53 health check</HealthCheckId>
--       </ResourceRecordSet>
--       ...
--    </ResourceRecordSets>
--    <IsTruncated>true | false</IsTruncated>
--    <MaxItems>value of maxitems parameter in the previous request</MaxItems>
--    <NextRecordName>if IsTruncated is true,
--       the DNS domain name of the first resource record set
--       in the next group of maxitems resource record sets</NextRecordName>
--    <NextRecordType>if IsTruncated is true,
--       the DNS record type of the first resource record set
--       in the next group of maxitems resource record sets</NextRecordType>
--    <NextRecordIdentifier>if IsTruncated is true
--       and results were truncated for a weighted, latency, or failover
--       resource record set, the value of SetIdentifier for the
--       first resource record set in the next group of maxitems
--       resource record sets</NextRecordIdentifier>
-- </ListResourceRecordSetsResponse>

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    
    deriving (Show)

$(deriveJSON fieldOptions ''ListResourceRecordSetsResponse)

-- <GetChangeResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <ChangeInfo>
--       <Id>unique identifier for the change batch request</Id>
--       <Status>PENDING | INSYNC</Status>
--       <SubmittedAt>date and time in Coordinated Universal Time
--          format</SubmittedAt>
--    </ChangeInfo>
-- </GetChangeResponse>

data GetChangeResponse = GetChangeResponse
    { gcrChangeInfo :: !ChangeInfo
    } deriving (Show)

$(deriveJSON fieldOptions ''GetChangeResponse)

--
-- Health Checks
--

data HealthCheckConfig = HealthCheckConfig
    { hccIPAddress                :: !Text
    , hccPort                     :: !Int
    , hccType                     :: !Protocol
    , hccResourcePath             :: !Text
    , hccFullyQualifiedDomainName :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''HealthCheckConfig)

data HealthCheck = HealthCheck
    { hcId                :: !Text
    , hcCallerRef         :: !CallerRef
    , hcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Show)

$(deriveJSON fieldOptions ''HealthCheck)

-- <CreateHealthCheckResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HealthCheck>
--       <Id>ID that Route 53 assigned to the new health check</Id>
--       <CallerReference>unique description</CallerReference>
--       <HealthCheckConfig>
--          <IPAddress>IP address of the endpoint to check</IPAddress>
--          <Port>port on the endpoint to check</Port>
--          <Type>HTTP | TCP</Type>
--          <ResourcePath>path of the file that you want Route 53 to request</ResourcePath>
--          <FullyQualifiedDomainName>domain name of the endpoint to check</FullyQualifiedDomainName>
--       </HealthCheckConfig>
--    </HealthCheck>
-- </CreateHealthCheckResponse>

type CreateHealthCheckResponse = HealthCheck

-- <GetHealthCheckResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HealthCheck>
--       <Id>health check ID</Id>
--       <CallerReference>unique description</CallerReference>
--       <HealthCheckConfig>
--          <IPAddress>IP address of the endpoint to check</IPAddress>
--          <Port>port on the endpoint to check</Port>
--          <Type>HTTP | TCP</Type>
--          <ResourcePath>path of the file that you want Route 53 to request</ResourcePath>
--          <FullyQualifiedDomainName>domain name of the endpoint to check</FullyQualifiedDomainName>
--       </HealthCheckConfig>
--    </HealthCheck>
-- </GetHealthCheckResponse>

type GetHealthCheckResponse = HealthCheck

-- <ListHealthChecksResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <HealthChecks>
--       <HealthCheck>
--          <Id>abcdef11-2222-3333-4444-555555fedcba</Id>
--          <CallerReference>example.com 192.0.2.17</CallerReference>
--          <HealthCheckConfig>
--             <IPAddress>192.0.2.17</IPAddress>
--             <Port>80</Port>
--             <Type>HTTP</Type>
--             <ResourcePath>/docs/route-53-health-check.html</ResourcePath>
--             <FullyQualifiedDomainName>example.com</FullyQualifiedDomainName>
--          </HealthCheckConfig>
--       </HealthCheck>
--       ...
--    </HealthChecks>
--    <IsTruncated>true</IsTruncated>
--    <NextMarker>aaaaaaaa-1234-5678-9012-bbbbbbcccccc</NextMarker>
--    <MaxItems>1</MaxItems>
-- </ListHealthChecksResponse>

data ListHealthChecksResponse = ListHealthChecksResponse
    { lhcrIsTruncated  :: !Text
    , lhcrHealthChecks :: ![HealthCheck]
    , lhcrMaxItems     :: !Text
    , lhcrMarker       :: !(Maybe Text)
    , lhcrNextMarker   :: !(Maybe Text)
    } deriving (Show)

-- instance FromJSON ListHealthChecksResponse where
--     parseJSON (Object o) = ListHealthChecksResponse
--         <$> o .: "IsTruncated"
--         <*> o .: "MaxItems"
--     parseJSON _ = mzero

$(deriveJSON fieldOptions ''ListHealthChecksResponse)

-- <DeleteHealthCheckResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
-- </DeleteHealthCheckResponse>

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Show)

$(deriveJSON defaultOptions ''DeleteHealthCheckResponse)
