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
-- Module      : Network.AWS.Organizations.InviteAccountToOrganization
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an invitation to another account to join your organization as a member account. Organizations sends email on your behalf to the email address that is associated with the other account's owner. The invitation is implemented as a 'Handshake' whose details are in the response.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.InviteAccountToOrganization
    (
    -- * Creating a Request
      inviteAccountToOrganization
    , InviteAccountToOrganization
    -- * Request Lenses
    , iatoNotes
    , iatoTarget

    -- * Destructuring the Response
    , inviteAccountToOrganizationResponse
    , InviteAccountToOrganizationResponse
    -- * Response Lenses
    , iatorsHandshake
    , iatorsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Organizations.Types
import           Network.AWS.Organizations.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'inviteAccountToOrganization' smart constructor.
data InviteAccountToOrganization = InviteAccountToOrganization'
    { _iatoNotes  :: !(Maybe (Sensitive Text))
    , _iatoTarget :: !HandshakeParty
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InviteAccountToOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iatoNotes' - Additional information that you want to include in the generated email to the recipient account owner.
--
-- * 'iatoTarget' - The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:  @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@  If you use the AWS CLI, you can submit this as a single string, similar to the following example: @--target id=123456789012,type=ACCOUNT@  If you specify @"Type": "ACCOUNT"@ , then you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , then you must specify the email address that is associated with the account. @--target id=bill@example.com,type=EMAIL@
inviteAccountToOrganization
    :: HandshakeParty -- ^ 'iatoTarget'
    -> InviteAccountToOrganization
inviteAccountToOrganization pTarget_ =
    InviteAccountToOrganization'
    { _iatoNotes = Nothing
    , _iatoTarget = pTarget_
    }

-- | Additional information that you want to include in the generated email to the recipient account owner.
iatoNotes :: Lens' InviteAccountToOrganization (Maybe Text)
iatoNotes = lens _iatoNotes (\ s a -> s{_iatoNotes = a}) . mapping _Sensitive;

-- | The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:  @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@  If you use the AWS CLI, you can submit this as a single string, similar to the following example: @--target id=123456789012,type=ACCOUNT@  If you specify @"Type": "ACCOUNT"@ , then you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , then you must specify the email address that is associated with the account. @--target id=bill@example.com,type=EMAIL@
iatoTarget :: Lens' InviteAccountToOrganization HandshakeParty
iatoTarget = lens _iatoTarget (\ s a -> s{_iatoTarget = a});

instance AWSRequest InviteAccountToOrganization where
        type Rs InviteAccountToOrganization =
             InviteAccountToOrganizationResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 InviteAccountToOrganizationResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable InviteAccountToOrganization

instance NFData InviteAccountToOrganization

instance ToHeaders InviteAccountToOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.InviteAccountToOrganization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON InviteAccountToOrganization where
        toJSON InviteAccountToOrganization'{..}
          = object
              (catMaybes
                 [("Notes" .=) <$> _iatoNotes,
                  Just ("Target" .= _iatoTarget)])

instance ToPath InviteAccountToOrganization where
        toPath = const "/"

instance ToQuery InviteAccountToOrganization where
        toQuery = const mempty

-- | /See:/ 'inviteAccountToOrganizationResponse' smart constructor.
data InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse'
    { _iatorsHandshake      :: !(Maybe Handshake)
    , _iatorsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InviteAccountToOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iatorsHandshake' - A structure that contains details about the handshake that is created to support this invitation request.
--
-- * 'iatorsResponseStatus' - -- | The response status code.
inviteAccountToOrganizationResponse
    :: Int -- ^ 'iatorsResponseStatus'
    -> InviteAccountToOrganizationResponse
inviteAccountToOrganizationResponse pResponseStatus_ =
    InviteAccountToOrganizationResponse'
    { _iatorsHandshake = Nothing
    , _iatorsResponseStatus = pResponseStatus_
    }

-- | A structure that contains details about the handshake that is created to support this invitation request.
iatorsHandshake :: Lens' InviteAccountToOrganizationResponse (Maybe Handshake)
iatorsHandshake = lens _iatorsHandshake (\ s a -> s{_iatorsHandshake = a});

-- | -- | The response status code.
iatorsResponseStatus :: Lens' InviteAccountToOrganizationResponse Int
iatorsResponseStatus = lens _iatorsResponseStatus (\ s a -> s{_iatorsResponseStatus = a});

instance NFData InviteAccountToOrganizationResponse
