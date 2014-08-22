{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the specified domain contact's privacy setting. When
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
module Network.AWS.Route53Domains.V2014_05_15.UpdateDomainContactPrivacy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.V2014_05_15.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateDomainContactPrivacy' request.
updateDomainContactPrivacy :: Text -- ^ '_udcprDomainName'
                           -> UpdateDomainContactPrivacy
updateDomainContactPrivacy p1 = UpdateDomainContactPrivacy
    { _udcprDomainName = p1
    , _udcprTechPrivacy = Nothing
    , _udcprRegistrantPrivacy = Nothing
    , _udcprAdminPrivacy = Nothing
    }

data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy
    { _udcprDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    , _udcprTechPrivacy :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: None Valid values: true | false Required: No.
    , _udcprRegistrantPrivacy :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: None Valid values: true | false Required: No.
    , _udcprAdminPrivacy :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: None Valid values: true | false Required: No.
    } deriving (Show, Generic)

makeLenses ''UpdateDomainContactPrivacy

instance ToPath UpdateDomainContactPrivacy

instance ToQuery UpdateDomainContactPrivacy

instance ToHeaders UpdateDomainContactPrivacy

instance ToJSON UpdateDomainContactPrivacy

data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse
    { _udcpsOperationId :: Text
      -- ^ Identifier for tracking the progress of the request. To use this
      -- ID to query the operation status, use GetOperationDetail. Type:
      -- String Default: None Constraints: Maximum 255 characters.
    } deriving (Show, Generic)

makeLenses ''UpdateDomainContactPrivacyResponse

instance FromJSON UpdateDomainContactPrivacyResponse

instance AWSRequest UpdateDomainContactPrivacy where
    type Sv UpdateDomainContactPrivacy = Route53Domains
    type Rs UpdateDomainContactPrivacy = UpdateDomainContactPrivacyResponse

    request = get
    response _ = jsonResponse
