{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityMailFromDomainAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.BehaviorOnMXFailure
import Network.AWS.SES.Types.CustomMailFromStatus

-- | Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).
--
--
--
-- /See:/ 'identityMailFromDomainAttributes' smart constructor.
data IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes'
  { _imfdaMailFromDomain ::
      !Text,
    _imfdaMailFromDomainStatus ::
      !CustomMailFromStatus,
    _imfdaBehaviorOnMXFailure ::
      !BehaviorOnMXFailure
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityMailFromDomainAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imfdaMailFromDomain' - The custom MAIL FROM domain that the identity is configured to use.
--
-- * 'imfdaMailFromDomainStatus' - The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
--
-- * 'imfdaBehaviorOnMXFailure' - The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email. The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
identityMailFromDomainAttributes ::
  -- | 'imfdaMailFromDomain'
  Text ->
  -- | 'imfdaMailFromDomainStatus'
  CustomMailFromStatus ->
  -- | 'imfdaBehaviorOnMXFailure'
  BehaviorOnMXFailure ->
  IdentityMailFromDomainAttributes
identityMailFromDomainAttributes
  pMailFromDomain_
  pMailFromDomainStatus_
  pBehaviorOnMXFailure_ =
    IdentityMailFromDomainAttributes'
      { _imfdaMailFromDomain =
          pMailFromDomain_,
        _imfdaMailFromDomainStatus = pMailFromDomainStatus_,
        _imfdaBehaviorOnMXFailure = pBehaviorOnMXFailure_
      }

-- | The custom MAIL FROM domain that the identity is configured to use.
imfdaMailFromDomain :: Lens' IdentityMailFromDomainAttributes Text
imfdaMailFromDomain = lens _imfdaMailFromDomain (\s a -> s {_imfdaMailFromDomain = a})

-- | The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
imfdaMailFromDomainStatus :: Lens' IdentityMailFromDomainAttributes CustomMailFromStatus
imfdaMailFromDomainStatus = lens _imfdaMailFromDomainStatus (\s a -> s {_imfdaMailFromDomainStatus = a})

-- | The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email. The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
imfdaBehaviorOnMXFailure :: Lens' IdentityMailFromDomainAttributes BehaviorOnMXFailure
imfdaBehaviorOnMXFailure = lens _imfdaBehaviorOnMXFailure (\s a -> s {_imfdaBehaviorOnMXFailure = a})

instance FromXML IdentityMailFromDomainAttributes where
  parseXML x =
    IdentityMailFromDomainAttributes'
      <$> (x .@ "MailFromDomain")
      <*> (x .@ "MailFromDomainStatus")
      <*> (x .@ "BehaviorOnMXFailure")

instance Hashable IdentityMailFromDomainAttributes

instance NFData IdentityMailFromDomainAttributes
