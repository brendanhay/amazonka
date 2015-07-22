{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation checks the availability of one domain name. You can
-- access this API without authenticating. Note that if the availability
-- status of a domain is pending, you must submit another request to
-- determine the availability of the domain name.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-CheckDomainAvailability.html>
module Network.AWS.Route53Domains.CheckDomainAvailability
    (
    -- * Request
      CheckDomainAvailability
    -- ** Request constructor
    , checkDomainAvailability
    -- ** Request lenses
    , cdarqIdNLangCode
    , cdarqDomainName

    -- * Response
    , CheckDomainAvailabilityResponse
    -- ** Response constructor
    , checkDomainAvailabilityResponse
    -- ** Response lenses
    , cdarsStatus
    , cdarsAvailability
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The CheckDomainAvailability request contains the following elements.
--
-- /See:/ 'checkDomainAvailability' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdarqIdNLangCode'
--
-- * 'cdarqDomainName'
data CheckDomainAvailability = CheckDomainAvailability'
    { _cdarqIdNLangCode :: !(Maybe Text)
    , _cdarqDomainName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CheckDomainAvailability' smart constructor.
checkDomainAvailability :: Text -> CheckDomainAvailability
checkDomainAvailability pDomainName_ =
    CheckDomainAvailability'
    { _cdarqIdNLangCode = Nothing
    , _cdarqDomainName = pDomainName_
    }

-- | Reserved for future use.
cdarqIdNLangCode :: Lens' CheckDomainAvailability (Maybe Text)
cdarqIdNLangCode = lens _cdarqIdNLangCode (\ s a -> s{_cdarqIdNLangCode = a});

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
cdarqDomainName :: Lens' CheckDomainAvailability Text
cdarqDomainName = lens _cdarqDomainName (\ s a -> s{_cdarqDomainName = a});

instance AWSRequest CheckDomainAvailability where
        type Sv CheckDomainAvailability = Route53Domains
        type Rs CheckDomainAvailability =
             CheckDomainAvailabilityResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CheckDomainAvailabilityResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Availability"))

instance ToHeaders CheckDomainAvailability where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.CheckDomainAvailability"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CheckDomainAvailability where
        toJSON CheckDomainAvailability'{..}
          = object
              ["IdnLangCode" .= _cdarqIdNLangCode,
               "DomainName" .= _cdarqDomainName]

instance ToPath CheckDomainAvailability where
        toPath = const "/"

instance ToQuery CheckDomainAvailability where
        toQuery = const mempty

-- | The CheckDomainAvailability response includes the following elements.
--
-- /See:/ 'checkDomainAvailabilityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdarsStatus'
--
-- * 'cdarsAvailability'
data CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse'
    { _cdarsStatus       :: !Int
    , _cdarsAvailability :: !DomainAvailability
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CheckDomainAvailabilityResponse' smart constructor.
checkDomainAvailabilityResponse :: Int -> DomainAvailability -> CheckDomainAvailabilityResponse
checkDomainAvailabilityResponse pStatus_ pAvailability_ =
    CheckDomainAvailabilityResponse'
    { _cdarsStatus = pStatus_
    , _cdarsAvailability = pAvailability_
    }

-- | FIXME: Undocumented member.
cdarsStatus :: Lens' CheckDomainAvailabilityResponse Int
cdarsStatus = lens _cdarsStatus (\ s a -> s{_cdarsStatus = a});

-- | Whether the domain name is available for registering.
--
-- You can only register domains designated as @AVAILABLE@.
--
-- Type: String
--
-- Valid values:
--
-- -   @AVAILABLE@ – The domain name is available.
-- -   @AVAILABLE_RESERVED@ – The domain name is reserved under specific
--     conditions.
-- -   @AVAILABLE_PREORDER@ – The domain name is available and can be
--     preordered.
-- -   @UNAVAILABLE@ – The domain name is not available.
-- -   @UNAVAILABLE_PREMIUM@ – The domain name is not available.
-- -   @UNAVAILABLE_RESTRICTED@ – The domain name is forbidden.
-- -   @RESERVED@ – The domain name has been reserved for another person or
--     organization.
-- -   @DONT_KNOW@ – The TLD registry didn\'t reply with a definitive
--     answer about whether the domain name is available. Amazon Route 53
--     can return this response for a variety of reasons, for example, the
--     registry is performing maintenance. Try again later.
cdarsAvailability :: Lens' CheckDomainAvailabilityResponse DomainAvailability
cdarsAvailability = lens _cdarsAvailability (\ s a -> s{_cdarsAvailability = a});
