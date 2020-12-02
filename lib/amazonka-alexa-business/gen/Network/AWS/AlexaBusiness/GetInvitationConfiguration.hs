{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetInvitationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configured values for the user enrollment invitation email template.
module Network.AWS.AlexaBusiness.GetInvitationConfiguration
  ( -- * Creating a Request
    getInvitationConfiguration,
    GetInvitationConfiguration,

    -- * Destructuring the Response
    getInvitationConfigurationResponse,
    GetInvitationConfigurationResponse,

    -- * Response Lenses
    gicrsContactEmail,
    gicrsOrganizationName,
    gicrsPrivateSkillIds,
    gicrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInvitationConfiguration' smart constructor.
data GetInvitationConfiguration = GetInvitationConfiguration'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInvitationConfiguration' with the minimum fields required to make a request.
getInvitationConfiguration ::
  GetInvitationConfiguration
getInvitationConfiguration = GetInvitationConfiguration'

instance AWSRequest GetInvitationConfiguration where
  type
    Rs GetInvitationConfiguration =
      GetInvitationConfigurationResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          GetInvitationConfigurationResponse'
            <$> (x .?> "ContactEmail")
            <*> (x .?> "OrganizationName")
            <*> (x .?> "PrivateSkillIds" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetInvitationConfiguration

instance NFData GetInvitationConfiguration

instance ToHeaders GetInvitationConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.GetInvitationConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetInvitationConfiguration where
  toJSON = const (Object mempty)

instance ToPath GetInvitationConfiguration where
  toPath = const "/"

instance ToQuery GetInvitationConfiguration where
  toQuery = const mempty

-- | /See:/ 'getInvitationConfigurationResponse' smart constructor.
data GetInvitationConfigurationResponse = GetInvitationConfigurationResponse'
  { _gicrsContactEmail ::
      !(Maybe Text),
    _gicrsOrganizationName ::
      !(Maybe Text),
    _gicrsPrivateSkillIds ::
      !(Maybe [Text]),
    _gicrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInvitationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gicrsContactEmail' - The email ID of the organization or individual contact that the enrolled user can use.
--
-- * 'gicrsOrganizationName' - The name of the organization sending the enrollment invite to a user.
--
-- * 'gicrsPrivateSkillIds' - The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- * 'gicrsResponseStatus' - -- | The response status code.
getInvitationConfigurationResponse ::
  -- | 'gicrsResponseStatus'
  Int ->
  GetInvitationConfigurationResponse
getInvitationConfigurationResponse pResponseStatus_ =
  GetInvitationConfigurationResponse'
    { _gicrsContactEmail = Nothing,
      _gicrsOrganizationName = Nothing,
      _gicrsPrivateSkillIds = Nothing,
      _gicrsResponseStatus = pResponseStatus_
    }

-- | The email ID of the organization or individual contact that the enrolled user can use.
gicrsContactEmail :: Lens' GetInvitationConfigurationResponse (Maybe Text)
gicrsContactEmail = lens _gicrsContactEmail (\s a -> s {_gicrsContactEmail = a})

-- | The name of the organization sending the enrollment invite to a user.
gicrsOrganizationName :: Lens' GetInvitationConfigurationResponse (Maybe Text)
gicrsOrganizationName = lens _gicrsOrganizationName (\s a -> s {_gicrsOrganizationName = a})

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
gicrsPrivateSkillIds :: Lens' GetInvitationConfigurationResponse [Text]
gicrsPrivateSkillIds = lens _gicrsPrivateSkillIds (\s a -> s {_gicrsPrivateSkillIds = a}) . _Default . _Coerce

-- | -- | The response status code.
gicrsResponseStatus :: Lens' GetInvitationConfigurationResponse Int
gicrsResponseStatus = lens _gicrsResponseStatus (\s a -> s {_gicrsResponseStatus = a})

instance NFData GetInvitationConfigurationResponse
