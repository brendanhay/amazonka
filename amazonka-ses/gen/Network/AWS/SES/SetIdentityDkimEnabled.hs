{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Enables or disables Easy DKIM signing of email sent from an identity:
--
-- -   If Easy DKIM signing is enabled for a domain name identity (e.g.,
--     @example.com@), then Amazon SES will DKIM-sign all email sent by
--     addresses under that domain name (e.g., @user\@example.com@).
-- -   If Easy DKIM signing is enabled for an email address, then Amazon
--     SES will DKIM-sign all email sent by that email address.
--
-- For email addresses (e.g., @user\@example.com@), you can only enable
-- Easy DKIM signing if the corresponding domain (e.g., @example.com@) has
-- been set up for Easy DKIM using the AWS Console or the
-- @VerifyDomainDkim@ action.
--
-- This action is throttled at one request per second.
--
-- For more information about Easy DKIM signing, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityDkimEnabled.html>
module Network.AWS.SES.SetIdentityDkimEnabled
    (
    -- * Request
      SetIdentityDkimEnabled
    -- ** Request constructor
    , setIdentityDkimEnabled
    -- ** Request lenses
    , sideIdentity
    , sideDkimEnabled

    -- * Response
    , SetIdentityDkimEnabledResponse
    -- ** Response constructor
    , setIdentityDkimEnabledResponse
    -- ** Response lenses
    , siderStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to enable or disable DKIM
-- signing for an identity.
--
-- /See:/ 'setIdentityDkimEnabled' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sideIdentity'
--
-- * 'sideDkimEnabled'
data SetIdentityDkimEnabled = SetIdentityDkimEnabled'
    { _sideIdentity    :: !Text
    , _sideDkimEnabled :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetIdentityDkimEnabled' smart constructor.
setIdentityDkimEnabled :: Text -> Bool -> SetIdentityDkimEnabled
setIdentityDkimEnabled pIdentity pDkimEnabled =
    SetIdentityDkimEnabled'
    { _sideIdentity = pIdentity
    , _sideDkimEnabled = pDkimEnabled
    }

-- | The identity for which DKIM signing should be enabled or disabled.
sideIdentity :: Lens' SetIdentityDkimEnabled Text
sideIdentity = lens _sideIdentity (\ s a -> s{_sideIdentity = a});

-- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to
-- enable DKIM signing for this identity; @false@ to disable it.
sideDkimEnabled :: Lens' SetIdentityDkimEnabled Bool
sideDkimEnabled = lens _sideDkimEnabled (\ s a -> s{_sideDkimEnabled = a});

instance AWSRequest SetIdentityDkimEnabled where
        type Sv SetIdentityDkimEnabled = SES
        type Rs SetIdentityDkimEnabled =
             SetIdentityDkimEnabledResponse
        request = post
        response
          = receiveXMLWrapper "SetIdentityDkimEnabledResult"
              (\ s h x ->
                 SetIdentityDkimEnabledResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders SetIdentityDkimEnabled where
        toHeaders = const mempty

instance ToPath SetIdentityDkimEnabled where
        toPath = const "/"

instance ToQuery SetIdentityDkimEnabled where
        toQuery SetIdentityDkimEnabled'{..}
          = mconcat
              ["Action" =:
                 ("SetIdentityDkimEnabled" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _sideIdentity,
               "DkimEnabled" =: _sideDkimEnabled]

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'setIdentityDkimEnabledResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siderStatus'
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse'
    { _siderStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetIdentityDkimEnabledResponse' smart constructor.
setIdentityDkimEnabledResponse :: Int -> SetIdentityDkimEnabledResponse
setIdentityDkimEnabledResponse pStatus =
    SetIdentityDkimEnabledResponse'
    { _siderStatus = pStatus
    }

-- | FIXME: Undocumented member.
siderStatus :: Lens' SetIdentityDkimEnabledResponse Int
siderStatus = lens _siderStatus (\ s a -> s{_siderStatus = a});
