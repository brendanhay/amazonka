{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ResendContactReachabilityEmail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation resends the confirmation email to the current email address for the registrant contact.
--
--
module Network.AWS.Route53Domains.ResendContactReachabilityEmail
    (
    -- * Creating a Request
      resendContactReachabilityEmail
    , ResendContactReachabilityEmail
    -- * Request Lenses
    , rcreDomainName

    -- * Destructuring the Response
    , resendContactReachabilityEmailResponse
    , ResendContactReachabilityEmailResponse
    -- * Response Lenses
    , rcrersDomainName
    , rcrersEmailAddress
    , rcrersIsAlreadyVerified
    , rcrersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'resendContactReachabilityEmail' smart constructor.
newtype ResendContactReachabilityEmail = ResendContactReachabilityEmail'
  { _rcreDomainName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResendContactReachabilityEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcreDomainName' - The name of the domain for which you want Amazon Route 53 to resend a confirmation email to the registrant contact.
resendContactReachabilityEmail
    :: ResendContactReachabilityEmail
resendContactReachabilityEmail =
  ResendContactReachabilityEmail' {_rcreDomainName = Nothing}


-- | The name of the domain for which you want Amazon Route 53 to resend a confirmation email to the registrant contact.
rcreDomainName :: Lens' ResendContactReachabilityEmail (Maybe Text)
rcreDomainName = lens _rcreDomainName (\ s a -> s{_rcreDomainName = a})

instance AWSRequest ResendContactReachabilityEmail
         where
        type Rs ResendContactReachabilityEmail =
             ResendContactReachabilityEmailResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 ResendContactReachabilityEmailResponse' <$>
                   (x .?> "domainName") <*> (x .?> "emailAddress") <*>
                     (x .?> "isAlreadyVerified")
                     <*> (pure (fromEnum s)))

instance Hashable ResendContactReachabilityEmail
         where

instance NFData ResendContactReachabilityEmail where

instance ToHeaders ResendContactReachabilityEmail
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.ResendContactReachabilityEmail"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResendContactReachabilityEmail where
        toJSON ResendContactReachabilityEmail'{..}
          = object
              (catMaybes [("domainName" .=) <$> _rcreDomainName])

instance ToPath ResendContactReachabilityEmail where
        toPath = const "/"

instance ToQuery ResendContactReachabilityEmail where
        toQuery = const mempty

-- | /See:/ 'resendContactReachabilityEmailResponse' smart constructor.
data ResendContactReachabilityEmailResponse = ResendContactReachabilityEmailResponse'
  { _rcrersDomainName        :: !(Maybe Text)
  , _rcrersEmailAddress      :: !(Maybe Text)
  , _rcrersIsAlreadyVerified :: !(Maybe Bool)
  , _rcrersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResendContactReachabilityEmailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrersDomainName' - The domain name for which you requested a confirmation email.
--
-- * 'rcrersEmailAddress' - The email address for the registrant contact at the time that we sent the verification email.
--
-- * 'rcrersIsAlreadyVerified' - @True@ if the email address for the registrant contact has already been verified, and @false@ otherwise. If the email address has already been verified, we don't send another confirmation email.
--
-- * 'rcrersResponseStatus' - -- | The response status code.
resendContactReachabilityEmailResponse
    :: Int -- ^ 'rcrersResponseStatus'
    -> ResendContactReachabilityEmailResponse
resendContactReachabilityEmailResponse pResponseStatus_ =
  ResendContactReachabilityEmailResponse'
    { _rcrersDomainName = Nothing
    , _rcrersEmailAddress = Nothing
    , _rcrersIsAlreadyVerified = Nothing
    , _rcrersResponseStatus = pResponseStatus_
    }


-- | The domain name for which you requested a confirmation email.
rcrersDomainName :: Lens' ResendContactReachabilityEmailResponse (Maybe Text)
rcrersDomainName = lens _rcrersDomainName (\ s a -> s{_rcrersDomainName = a})

-- | The email address for the registrant contact at the time that we sent the verification email.
rcrersEmailAddress :: Lens' ResendContactReachabilityEmailResponse (Maybe Text)
rcrersEmailAddress = lens _rcrersEmailAddress (\ s a -> s{_rcrersEmailAddress = a})

-- | @True@ if the email address for the registrant contact has already been verified, and @false@ otherwise. If the email address has already been verified, we don't send another confirmation email.
rcrersIsAlreadyVerified :: Lens' ResendContactReachabilityEmailResponse (Maybe Bool)
rcrersIsAlreadyVerified = lens _rcrersIsAlreadyVerified (\ s a -> s{_rcrersIsAlreadyVerified = a})

-- | -- | The response status code.
rcrersResponseStatus :: Lens' ResendContactReachabilityEmailResponse Int
rcrersResponseStatus = lens _rcrersResponseStatus (\ s a -> s{_rcrersResponseStatus = a})

instance NFData
           ResendContactReachabilityEmailResponse
         where
