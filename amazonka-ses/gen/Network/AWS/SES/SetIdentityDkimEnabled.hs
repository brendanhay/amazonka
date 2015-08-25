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
-- Module      : Network.AWS.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables Easy DKIM signing of email sent from an identity:
--
-- -   If Easy DKIM signing is enabled for a domain name identity (e.g.,
--     'example.com'), then Amazon SES will DKIM-sign all email sent by
--     addresses under that domain name (e.g., 'user\'example.com').
-- -   If Easy DKIM signing is enabled for an email address, then Amazon
--     SES will DKIM-sign all email sent by that email address.
--
-- For email addresses (e.g., 'user\'example.com'), you can only enable
-- Easy DKIM signing if the corresponding domain (e.g., 'example.com') has
-- been set up for Easy DKIM using the AWS Console or the
-- 'VerifyDomainDkim' action.
--
-- This action is throttled at one request per second.
--
-- For more information about Easy DKIM signing, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityDkimEnabled.html AWS API Reference> for SetIdentityDkimEnabled.
module Network.AWS.SES.SetIdentityDkimEnabled
    (
    -- * Creating a Request
      setIdentityDkimEnabled
    , SetIdentityDkimEnabled
    -- * Request Lenses
    , sideIdentity
    , sideDkimEnabled

    -- * Destructuring the Response
    , setIdentityDkimEnabledResponse
    , SetIdentityDkimEnabledResponse
    -- * Response Lenses
    , sidersStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request instructing the service to enable or disable DKIM
-- signing for an identity.
--
-- /See:/ 'setIdentityDkimEnabled' smart constructor.
data SetIdentityDkimEnabled = SetIdentityDkimEnabled'
    { _sideIdentity    :: !Text
    , _sideDkimEnabled :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityDkimEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sideIdentity'
--
-- * 'sideDkimEnabled'
setIdentityDkimEnabled
    :: Text -- ^ 'sideIdentity'
    -> Bool -- ^ 'sideDkimEnabled'
    -> SetIdentityDkimEnabled
setIdentityDkimEnabled pIdentity_ pDkimEnabled_ =
    SetIdentityDkimEnabled'
    { _sideIdentity = pIdentity_
    , _sideDkimEnabled = pDkimEnabled_
    }

-- | The identity for which DKIM signing should be enabled or disabled.
sideIdentity :: Lens' SetIdentityDkimEnabled Text
sideIdentity = lens _sideIdentity (\ s a -> s{_sideIdentity = a});

-- | Sets whether DKIM signing is enabled for an identity. Set to 'true' to
-- enable DKIM signing for this identity; 'false' to disable it.
sideDkimEnabled :: Lens' SetIdentityDkimEnabled Bool
sideDkimEnabled = lens _sideDkimEnabled (\ s a -> s{_sideDkimEnabled = a});

instance AWSRequest SetIdentityDkimEnabled where
        type Rs SetIdentityDkimEnabled =
             SetIdentityDkimEnabledResponse
        request = postQuery sES
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
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse'
    { _sidersStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIdentityDkimEnabledResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sidersStatus'
setIdentityDkimEnabledResponse
    :: Int -- ^ 'sidersStatus'
    -> SetIdentityDkimEnabledResponse
setIdentityDkimEnabledResponse pStatus_ =
    SetIdentityDkimEnabledResponse'
    { _sidersStatus = pStatus_
    }

-- | The response status code.
sidersStatus :: Lens' SetIdentityDkimEnabledResponse Int
sidersStatus = lens _sidersStatus (\ s a -> s{_sidersStatus = a});
