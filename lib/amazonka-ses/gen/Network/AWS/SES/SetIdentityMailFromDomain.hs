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
-- Module      : Network.AWS.SES.SetIdentityMailFromDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the custom MAIL FROM domain setup for a verified identity (an email address or a domain).
--
--
-- /Important:/ To send emails using the specified MAIL FROM domain, you must add an MX record to your MAIL FROM domain's DNS settings. If you want your emails to pass Sender Policy Framework (SPF) checks, you must also add or update an SPF record. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.SetIdentityMailFromDomain
    (
    -- * Creating a Request
      setIdentityMailFromDomain
    , SetIdentityMailFromDomain
    -- * Request Lenses
    , simfdMailFromDomain
    , simfdBehaviorOnMXFailure
    , simfdIdentity

    -- * Destructuring the Response
    , setIdentityMailFromDomainResponse
    , SetIdentityMailFromDomainResponse
    -- * Response Lenses
    , simfdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to enable or disable the Amazon SES custom MAIL FROM domain setup for a verified identity. For information about using a custom MAIL FROM domain, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'setIdentityMailFromDomain' smart constructor.
data SetIdentityMailFromDomain = SetIdentityMailFromDomain'
  { _simfdMailFromDomain      :: !(Maybe Text)
  , _simfdBehaviorOnMXFailure :: !(Maybe BehaviorOnMXFailure)
  , _simfdIdentity            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityMailFromDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simfdMailFromDomain' - The custom MAIL FROM domain that you want the verified identity to use. The MAIL FROM domain must 1) be a subdomain of the verified identity, 2) not be used in a "From" address if the MAIL FROM domain is the destination of email feedback forwarding (for more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> ), and 3) not be used to receive emails. A value of @null@ disables the custom MAIL FROM setting for the identity.
--
-- * 'simfdBehaviorOnMXFailure' - The action that you want Amazon SES to take if it cannot successfully read the required MX record when you send an email. If you choose @UseDefaultValue@ , Amazon SES will use amazonses.com (or a subdomain of that) as the MAIL FROM domain. If you choose @RejectMessage@ , Amazon SES will return a @MailFromDomainNotVerified@ error and not send the email. The action specified in @BehaviorOnMXFailure@ is taken when the custom MAIL FROM domain setup is in the @Pending@ , @Failed@ , and @TemporaryFailure@ states.
--
-- * 'simfdIdentity' - The verified identity for which you want to enable or disable the specified custom MAIL FROM domain.
setIdentityMailFromDomain
    :: Text -- ^ 'simfdIdentity'
    -> SetIdentityMailFromDomain
setIdentityMailFromDomain pIdentity_ =
  SetIdentityMailFromDomain'
    { _simfdMailFromDomain = Nothing
    , _simfdBehaviorOnMXFailure = Nothing
    , _simfdIdentity = pIdentity_
    }


-- | The custom MAIL FROM domain that you want the verified identity to use. The MAIL FROM domain must 1) be a subdomain of the verified identity, 2) not be used in a "From" address if the MAIL FROM domain is the destination of email feedback forwarding (for more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> ), and 3) not be used to receive emails. A value of @null@ disables the custom MAIL FROM setting for the identity.
simfdMailFromDomain :: Lens' SetIdentityMailFromDomain (Maybe Text)
simfdMailFromDomain = lens _simfdMailFromDomain (\ s a -> s{_simfdMailFromDomain = a})

-- | The action that you want Amazon SES to take if it cannot successfully read the required MX record when you send an email. If you choose @UseDefaultValue@ , Amazon SES will use amazonses.com (or a subdomain of that) as the MAIL FROM domain. If you choose @RejectMessage@ , Amazon SES will return a @MailFromDomainNotVerified@ error and not send the email. The action specified in @BehaviorOnMXFailure@ is taken when the custom MAIL FROM domain setup is in the @Pending@ , @Failed@ , and @TemporaryFailure@ states.
simfdBehaviorOnMXFailure :: Lens' SetIdentityMailFromDomain (Maybe BehaviorOnMXFailure)
simfdBehaviorOnMXFailure = lens _simfdBehaviorOnMXFailure (\ s a -> s{_simfdBehaviorOnMXFailure = a})

-- | The verified identity for which you want to enable or disable the specified custom MAIL FROM domain.
simfdIdentity :: Lens' SetIdentityMailFromDomain Text
simfdIdentity = lens _simfdIdentity (\ s a -> s{_simfdIdentity = a})

instance AWSRequest SetIdentityMailFromDomain where
        type Rs SetIdentityMailFromDomain =
             SetIdentityMailFromDomainResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SetIdentityMailFromDomainResult"
              (\ s h x ->
                 SetIdentityMailFromDomainResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetIdentityMailFromDomain where

instance NFData SetIdentityMailFromDomain where

instance ToHeaders SetIdentityMailFromDomain where
        toHeaders = const mempty

instance ToPath SetIdentityMailFromDomain where
        toPath = const "/"

instance ToQuery SetIdentityMailFromDomain where
        toQuery SetIdentityMailFromDomain'{..}
          = mconcat
              ["Action" =:
                 ("SetIdentityMailFromDomain" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "MailFromDomain" =: _simfdMailFromDomain,
               "BehaviorOnMXFailure" =: _simfdBehaviorOnMXFailure,
               "Identity" =: _simfdIdentity]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'setIdentityMailFromDomainResponse' smart constructor.
newtype SetIdentityMailFromDomainResponse = SetIdentityMailFromDomainResponse'
  { _simfdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIdentityMailFromDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simfdrsResponseStatus' - -- | The response status code.
setIdentityMailFromDomainResponse
    :: Int -- ^ 'simfdrsResponseStatus'
    -> SetIdentityMailFromDomainResponse
setIdentityMailFromDomainResponse pResponseStatus_ =
  SetIdentityMailFromDomainResponse' {_simfdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
simfdrsResponseStatus :: Lens' SetIdentityMailFromDomainResponse Int
simfdrsResponseStatus = lens _simfdrsResponseStatus (\ s a -> s{_simfdrsResponseStatus = a})

instance NFData SetIdentityMailFromDomainResponse
         where
