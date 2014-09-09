{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Pending.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.Route53Domains.V2014_05_15.Trans
    (
    -- * CheckDomainAvailability
    -- $CheckDomainAvailability
      checkDomainAvailability

    -- * DisableDomainTransferLock
    -- $DisableDomainTransferLock
    , disableDomainTransferLock

    -- * EnableDomainTransferLock
    -- $EnableDomainTransferLock
    , enableDomainTransferLock

    -- * GetDomainDetail
    -- $GetDomainDetail
    , getDomainDetail

    -- * GetOperationDetail
    -- $GetOperationDetail
    , getOperationDetail

    -- * ListDomains
    -- $ListDomains
    , listDomains

    -- * ListOperations
    -- $ListOperations
    , listOperations

    -- * RegisterDomain
    -- $RegisterDomain
    , registerDomain

    -- * RetrieveDomainAuthCode
    -- $RetrieveDomainAuthCode
    , retrieveDomainAuthCode

    -- * TransferDomain
    -- $TransferDomain
    , transferDomain

    -- * UpdateDomainContact
    -- $UpdateDomainContact
    , updateDomainContact

    -- * UpdateDomainContactPrivacy
    -- $UpdateDomainContactPrivacy
    , updateDomainContactPrivacy

    -- * UpdateDomainNameservers
    -- $UpdateDomainNameservers
    , updateDomainNameservers

    -- * Re-exported
    , module AWS
    , module Network.AWS.Route53Domains.V2014_05_15
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.Route53Domains.V2014_05_15

-- $CheckDomainAvailability
-- This operation checks the availability of one domain name. You can access
-- this API without authenticating. Note that if the availability status of a
-- domain is pending, you must submit another request to determine the
-- availability of the domain name. CheckDomainAvailability Example POST /
-- HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205225Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.CheckDomainAvailability
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json- 1.1
-- content-length:[number of characters in the JSON string]
-- connections:Keep-Alive { "DomainName":"example.com" } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "Availability":"AVAILABLE" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.CheckDomainAvailability'

checkDomainAvailability :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => Text -- ^ 'cdaDomainName'
                        -> State CheckDomainAvailability a
                        -> m CheckDomainAvailabilityResponse
checkDomainAvailability p1 s =
    send $ (mkCheckDomainAvailability p1) &~ s

-- $DisableDomainTransferLock
-- This operation removes the transfer lock on the domain (specifically the
-- clientTransferProhibited status) to allow domain transfers. We recommend
-- you refrain from performing this action unless you intend to transfer the
-- domain to a different registrar. Successful submission returns an operation
-- ID that you can use to track the progress and completion of the action. If
-- the request is not completed successfully, the domain registrant will be
-- notified by email. DisableDomainTransferLock Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.DisableDomainTransferLock
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "OperationId":"0b370c79-faa4-40fe-94c8-b423069de3f6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.DisableDomainTransferLock'

disableDomainTransferLock :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
                          => Text -- ^ 'ddtlDomainName'
                          -> State DisableDomainTransferLock a
                          -> m DisableDomainTransferLockResponse
disableDomainTransferLock p1 s =
    send $ (mkDisableDomainTransferLock p1) &~ s

-- $EnableDomainTransferLock
-- This operation sets the transfer lock on the domain (specifically the
-- clientTransferProhibited status) to prevent domain transfers. Successful
-- submission returns an operation ID that you can use to track the progress
-- and completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email. EnableDomainTransferLock
-- Example POST / HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.EnableDomainTransferLock
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "OperationId":"0b370c79-faa4-40fe-94c8-b423069de3f6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.EnableDomainTransferLock'

enableDomainTransferLock :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
                         => Text -- ^ 'edtlDomainName'
                         -> State EnableDomainTransferLock a
                         -> m EnableDomainTransferLockResponse
enableDomainTransferLock p1 s =
    send $ (mkEnableDomainTransferLock p1) &~ s

-- $GetDomainDetail
-- This operation returns detailed information about the domain. The domain's
-- contact information is also returned as part of the output. GetDomainDetail
-- Example POST / HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.GetDomainDetail
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "AbuseContactEmail":"abuse@support.gandi.net",
-- "AbuseContactPhone":"+33.170377661", "AdminContact":{ "AddressLine1":"1 Any
-- Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "AdminPrivacy":true, "AutoRenew":true,
-- "CreationDate":1400010459, "DomainName":"example.com",
-- "ExpirationDate":1431539259, "Nameservers":[ { "GlueIps":[ ],
-- "Name":"ns-2048.awsdns-64.com" }, { "GlueIps":[ ],
-- "Name":"ns-2051.awsdns-67.co.uk" }, { "GlueIps":[ ],
-- "Name":"ns-2050.awsdns-66.org" }, { "GlueIps":[ ],
-- "Name":"ns-2049.awsdns-65.net" } ], "RegistrantContact":{ "AddressLine1":"1
-- Any Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "RegistrantPrivacy":true, "RegistrarName":"GANDI SAS",
-- "RegistrarUrl":"http://www.gandi.net", "Reseller":"Amazon", "StatusList":[
-- "clientTransferProhibited" ], "TechContact":{ "AddressLine1":"1 Any
-- Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "TechPrivacy":true, "UpdatedDate":1400010459,
-- "WhoIsServer":"whois.gandi.net" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.GetDomainDetail'

getDomainDetail :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
                => Text -- ^ 'gddDomainName'
                -> State GetDomainDetail a
                -> m GetDomainDetailResponse
getDomainDetail p1 s =
    send $ (mkGetDomainDetail p1) &~ s

-- $GetOperationDetail
-- This operation returns the current status of an operation that is not
-- completed. GetOperationDetail Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.GetOperationDetail
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "OperationId":"43884ce5-e30a-4801-858f-7aa86356c127" } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "DomainName":"happierdomain.ca",
-- "OperationId":"43884ce5-e30a-4801-858f-7aa86356c127",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate" : 1402630939.057, "Type" :
-- "REGISTER_DOMAIN" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.GetOperationDetail'

getOperationDetail :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => Text -- ^ 'godOperationId'
                   -> State GetOperationDetail a
                   -> m GetOperationDetailResponse
getOperationDetail p1 s =
    send $ (mkGetOperationDetail p1) &~ s

-- $ListDomains
-- This operation returns all the domain names registered with Amazon Route 53
-- for the current AWS account. ListDomains Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.ListDomains
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "Marker":"AxDAClaROQAXasf29GHWAIKPLA=", "MaxItems":20 } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] { "Domains":[ {
-- "AutoRenew":false, "DomainName":"example.com", "Expiry":1431203765,
-- "TransferLock":false }, { "AutoRenew":false, "DomainName":"example.net",
-- "Expiry":1431539260, "TransferLock":false }, { "AutoRenew":false,
-- "DomainName":"example.org", "Expiry":1431240024, "TransferLock":false }, {
-- "AutoRenew":false, "DomainName":"example.test", "Expiry":1431539259,
-- "TransferLock":false } ] }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.ListDomains'

listDomains :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => State ListDomains a
            -> m ListDomainsResponse
listDomains s =
    send (mkListDomains &~ s)

-- $ListOperations
-- This operation returns the operation IDs of operations that are not yet
-- complete. ListOperations Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.ListOperations
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] { "MaxItems" : 2 }
-- HTTP/1.1 200 Content-Length:[number of characters in the JSON string] {
-- "Operations":[ { "OperationId":"4ced3d4a-e011-45ee-b94f-1e2d73477562",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate":1403548979.088,
-- "Type":"CHANGE_PRIVACY_PROTECTION" }, {
-- "OperationId":"2e3ac45b-89b3-47ea-a042-f56dcd1b6883",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate":1403548986.429,
-- "Type":"DOMAIN_LOCK" } ] }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.ListOperations'

listOperations :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => State ListOperations a
               -> m ListOperationsResponse
listOperations s =
    send (mkListOperations &~ s)

-- $RegisterDomain
-- This operation registers a domain. Domains are registered by the AWS
-- registrar partner, Gandi. For some top-level domains (TLDs), this operation
-- requires extra parameters. When you register a domain, Amazon Route 53 does
-- the following: Creates a Amazon Route 53 hosted zone that has the same name
-- as the domain. Amazon Route 53 assigns four name servers to your hosted
-- zone and automatically updates your domain registration with the names of
-- these name servers. Enables autorenew, so your domain registration will
-- renew automatically each year. We'll notify you in advance of the renewal
-- date so you can choose whether to renew the registration. Optionally
-- enables privacy protection, so WHOIS queries return contact information for
-- our registrar partner, Gandi, instead of the information you entered for
-- registrant, admin, and tech contacts. If registration is successful,
-- returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully, the
-- domain registrant is notified by email. Charges your AWS account an amount
-- based on the top-level domain. For more information, see Amazon Route 53
-- Pricing. RegisterDomain Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.RegisterDomain
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "DurationInYears":1, "AutoRenew":true,
-- "AdminContact":{ "FirstName":"John", "MiddleName":"Richard",
-- "LastName":"Doe", "ContactType":"PERSON", "OrganizationName":"",
-- "AddressLine1":"123 Any Street", "AddressLine2":"", "City":"Any Town",
-- "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "RegistrantContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "TechContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "PrivacyProtectAdminContact":true,
-- "PrivacyProtectRegistrantContact":true, "PrivacyProtectTechContact":true }
-- HTTP/1.1 200 Content-Length:[number of characters in the JSON string] {
-- "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.RegisterDomain'

registerDomain :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'rdDomainName'
               -> Integer -- ^ 'rdDurationInYears'
               -> ContactDetail -- ^ 'rdAdminContact'
               -> ContactDetail -- ^ 'rdRegistrantContact'
               -> ContactDetail -- ^ 'rdTechContact'
               -> State RegisterDomain a
               -> m RegisterDomainResponse
registerDomain p1 p3 p5 p6 p7 s =
    send $ (mkRegisterDomain p1 p3 p5 p6 p7) &~ s

-- $RetrieveDomainAuthCode
-- This operation returns the AuthCode for the domain. To transfer a domain to
-- another registrar, you provide this value to the new registrar.
-- RetrieveDomainAuthCode Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.RetrieveDomainAuthCode
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] { "AuthCode":"rqL3*REjYH" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.RetrieveDomainAuthCode'

retrieveDomainAuthCode :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
                       => Text -- ^ 'rdacDomainName'
                       -> State RetrieveDomainAuthCode a
                       -> m RetrieveDomainAuthCodeResponse
retrieveDomainAuthCode p1 s =
    send $ (mkRetrieveDomainAuthCode p1) &~ s

-- $TransferDomain
-- This operation transfers a domain from another registrar to Amazon Route
-- 53. Domains are registered by the AWS registrar, Gandi upon transfer. To
-- transfer a domain, you need to meet all the domain transfer criteria,
-- including the following: You must supply nameservers to transfer a domain.
-- You must disable the domain transfer lock (if any) before transferring the
-- domain. A minimum of 60 days must have elapsed since the domain's
-- registration or last transfer. We recommend you use the Amazon Route 53 as
-- the DNS service for your domain. You can create a hosted zone in Amazon
-- Route 53 for your current domain before transferring your domain. Note that
-- upon transfer, the domain duration is extended for a year if not otherwise
-- specified. Autorenew is enabled by default. If the transfer is successful,
-- this method returns an operation ID that you can use to track the progress
-- and completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email. Transferring domains
-- charges your AWS account an amount based on the top-level domain. For more
-- information, see Amazon Route 53 Pricing. TransferDomain Example POST /
-- HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.TransferDomain
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "DurationInYears":1, "Nameservers":[ {
-- "Name":"ns-2048.awsdns-64.com", "GlueIps":[ "192.0.2.11" ] }, {
-- "Name":"ns-2049.awsdns-65.net", "GlueIps":[ "192.0.2.12" ] } ],
-- "AuthCode":"a42qxjz1", "AutoRenew":true, "AdminContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" }, "RegistrantContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" }, "TechContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" },
-- "PrivacyProtectAdminContact":true, "PrivacyProtectRegistrantContact":true,
-- "PrivacyProtectTechContact":true, } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.TransferDomain'

transferDomain :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'tdDomainName'
               -> Integer -- ^ 'tdDurationInYears'
               -> [Nameserver] -- ^ 'tdNameservers'
               -> ContactDetail -- ^ 'tdAdminContact'
               -> ContactDetail -- ^ 'tdRegistrantContact'
               -> ContactDetail -- ^ 'tdTechContact'
               -> State TransferDomain a
               -> m TransferDomainResponse
transferDomain p1 p3 p4 p7 p8 p9 s =
    send $ (mkTransferDomain p1 p3 p4 p7 p8 p9) &~ s

-- $UpdateDomainContact
-- This operation updates the contact information for a particular domain.
-- Information for at least one contact (registrant, administrator, or
-- technical) must be supplied for update. If the update is successful, this
-- method returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully, the
-- domain registrant will be notified by email. UpdateDomainContact Example
-- POST / HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.UpdateDomainContact
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "RegistrantContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "AdminContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "TechContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, } HTTP/1.1 200 Content-Length:[number of characters
-- in the JSON string] { "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6"
-- }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.UpdateDomainContact'

updateDomainContact :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => Text -- ^ 'udcDomainName'
                    -> State UpdateDomainContact a
                    -> m UpdateDomainContactResponse
updateDomainContact p1 s =
    send $ (mkUpdateDomainContact p1) &~ s

-- $UpdateDomainContactPrivacy
-- This operation updates the specified domain contact's privacy setting. When
-- the privacy option is enabled, personal information such as postal or email
-- address is hidden from the results of a public WHOIS query. The privacy
-- services are provided by the AWS registrar, Gandi. For more information,
-- see the Gandi privacy features. This operation only affects the privacy of
-- the specified contact type (registrant, administrator, or tech). Successful
-- acceptance returns an operation ID that you can use with GetOperationDetail
-- to track the progress and completion of the action. If the request is not
-- completed successfully, the domain registrant will be notified by email.
-- UpdateDomainContactPrivacy Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.UpdateDomainContactPrivacy
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "AdminPrivacy":true, "RegistrantPrivacy":true,
-- "TechPrivacy":true, } HTTP/1.1 200 Content-Length:[number of characters in
-- the JSON string] { "OperationId":"777bc5da-fbf7-482c-b2ba-8946884a7dd6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.UpdateDomainContactPrivacy'

updateDomainContactPrivacy :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
                           => Text -- ^ 'udcpDomainName'
                           -> State UpdateDomainContactPrivacy a
                           -> m UpdateDomainContactPrivacyResponse
updateDomainContactPrivacy p1 s =
    send $ (mkUpdateDomainContactPrivacy p1) &~ s

-- $UpdateDomainNameservers
-- This operation replaces the current set of name servers for the domain with
-- the specified set of name servers. If you use Amazon Route 53 as your DNS
-- service, specify the four name servers in the delegation set for the hosted
-- zone for the domain. If successful, this operation returns an operation ID
-- that you can use to track the progress and completion of the action. If the
-- request is not completed successfully, the domain registrant will be
-- notified by email. UpdateDomainNameservers Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.UpdateDomainNameservers
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "Nameservers":[ { "Name":"ns1.example.net" }, {
-- "Name":"ns1.example.com", "GlueIps":[ "192.0.2.44" ] } ] } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "OperationId":"0b370c79-faa4-40fe-94c8-b423069de3f6" }.
--
-- See: 'Network.AWS.Route53Domains.V2014_05_15.UpdateDomainNameservers'

updateDomainNameservers :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => Text -- ^ 'udnDomainName'
                        -> [Nameserver] -- ^ 'udnNameservers'
                        -> State UpdateDomainNameservers a
                        -> m UpdateDomainNameserversResponse
updateDomainNameservers p1 p2 s =
    send $ (mkUpdateDomainNameservers p1 p2) &~ s
