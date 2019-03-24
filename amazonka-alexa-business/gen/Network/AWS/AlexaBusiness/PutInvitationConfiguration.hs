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
-- Module      : Network.AWS.AlexaBusiness.PutInvitationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the email template for the user enrollment invitation with the specified attributes.
--
--
module Network.AWS.AlexaBusiness.PutInvitationConfiguration
    (
    -- * Creating a Request
      putInvitationConfiguration
    , PutInvitationConfiguration
    -- * Request Lenses
    , picContactEmail
    , picPrivateSkillIds
    , picOrganizationName

    -- * Destructuring the Response
    , putInvitationConfigurationResponse
    , PutInvitationConfigurationResponse
    -- * Response Lenses
    , picrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putInvitationConfiguration' smart constructor.
data PutInvitationConfiguration = PutInvitationConfiguration'
  { _picContactEmail     :: !(Maybe Text)
  , _picPrivateSkillIds  :: !(Maybe [Text])
  , _picOrganizationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInvitationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'picContactEmail' - The email ID of the organization or individual contact that the enrolled user can use.
--
-- * 'picPrivateSkillIds' - The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- * 'picOrganizationName' - The name of the organization sending the enrollment invite to a user.
putInvitationConfiguration
    :: Text -- ^ 'picOrganizationName'
    -> PutInvitationConfiguration
putInvitationConfiguration pOrganizationName_ =
  PutInvitationConfiguration'
    { _picContactEmail = Nothing
    , _picPrivateSkillIds = Nothing
    , _picOrganizationName = pOrganizationName_
    }


-- | The email ID of the organization or individual contact that the enrolled user can use.
picContactEmail :: Lens' PutInvitationConfiguration (Maybe Text)
picContactEmail = lens _picContactEmail (\ s a -> s{_picContactEmail = a})

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
picPrivateSkillIds :: Lens' PutInvitationConfiguration [Text]
picPrivateSkillIds = lens _picPrivateSkillIds (\ s a -> s{_picPrivateSkillIds = a}) . _Default . _Coerce

-- | The name of the organization sending the enrollment invite to a user.
picOrganizationName :: Lens' PutInvitationConfiguration Text
picOrganizationName = lens _picOrganizationName (\ s a -> s{_picOrganizationName = a})

instance AWSRequest PutInvitationConfiguration where
        type Rs PutInvitationConfiguration =
             PutInvitationConfigurationResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 PutInvitationConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutInvitationConfiguration where

instance NFData PutInvitationConfiguration where

instance ToHeaders PutInvitationConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.PutInvitationConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutInvitationConfiguration where
        toJSON PutInvitationConfiguration'{..}
          = object
              (catMaybes
                 [("ContactEmail" .=) <$> _picContactEmail,
                  ("PrivateSkillIds" .=) <$> _picPrivateSkillIds,
                  Just ("OrganizationName" .= _picOrganizationName)])

instance ToPath PutInvitationConfiguration where
        toPath = const "/"

instance ToQuery PutInvitationConfiguration where
        toQuery = const mempty

-- | /See:/ 'putInvitationConfigurationResponse' smart constructor.
newtype PutInvitationConfigurationResponse = PutInvitationConfigurationResponse'
  { _picrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInvitationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'picrsResponseStatus' - -- | The response status code.
putInvitationConfigurationResponse
    :: Int -- ^ 'picrsResponseStatus'
    -> PutInvitationConfigurationResponse
putInvitationConfigurationResponse pResponseStatus_ =
  PutInvitationConfigurationResponse' {_picrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
picrsResponseStatus :: Lens' PutInvitationConfigurationResponse Int
picrsResponseStatus = lens _picrsResponseStatus (\ s a -> s{_picrsResponseStatus = a})

instance NFData PutInvitationConfigurationResponse
         where
