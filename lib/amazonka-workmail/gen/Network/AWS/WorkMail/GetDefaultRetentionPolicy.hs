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
-- Module      : Network.AWS.WorkMail.GetDefaultRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the default retention policy details for the specified organization.
module Network.AWS.WorkMail.GetDefaultRetentionPolicy
  ( -- * Creating a Request
    getDefaultRetentionPolicy,
    GetDefaultRetentionPolicy,

    -- * Request Lenses
    gdrpOrganizationId,

    -- * Destructuring the Response
    getDefaultRetentionPolicyResponse,
    GetDefaultRetentionPolicyResponse,

    -- * Response Lenses
    gdrprsName,
    gdrprsId,
    gdrprsFolderConfigurations,
    gdrprsDescription,
    gdrprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'getDefaultRetentionPolicy' smart constructor.
newtype GetDefaultRetentionPolicy = GetDefaultRetentionPolicy'
  { _gdrpOrganizationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDefaultRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrpOrganizationId' - The organization ID.
getDefaultRetentionPolicy ::
  -- | 'gdrpOrganizationId'
  Text ->
  GetDefaultRetentionPolicy
getDefaultRetentionPolicy pOrganizationId_ =
  GetDefaultRetentionPolicy'
    { _gdrpOrganizationId =
        pOrganizationId_
    }

-- | The organization ID.
gdrpOrganizationId :: Lens' GetDefaultRetentionPolicy Text
gdrpOrganizationId = lens _gdrpOrganizationId (\s a -> s {_gdrpOrganizationId = a})

instance AWSRequest GetDefaultRetentionPolicy where
  type
    Rs GetDefaultRetentionPolicy =
      GetDefaultRetentionPolicyResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          GetDefaultRetentionPolicyResponse'
            <$> (x .?> "Name")
            <*> (x .?> "Id")
            <*> (x .?> "FolderConfigurations" .!@ mempty)
            <*> (x .?> "Description")
            <*> (pure (fromEnum s))
      )

instance Hashable GetDefaultRetentionPolicy

instance NFData GetDefaultRetentionPolicy

instance ToHeaders GetDefaultRetentionPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.GetDefaultRetentionPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDefaultRetentionPolicy where
  toJSON GetDefaultRetentionPolicy' {..} =
    object
      (catMaybes [Just ("OrganizationId" .= _gdrpOrganizationId)])

instance ToPath GetDefaultRetentionPolicy where
  toPath = const "/"

instance ToQuery GetDefaultRetentionPolicy where
  toQuery = const mempty

-- | /See:/ 'getDefaultRetentionPolicyResponse' smart constructor.
data GetDefaultRetentionPolicyResponse = GetDefaultRetentionPolicyResponse'
  { _gdrprsName ::
      !(Maybe Text),
    _gdrprsId ::
      !(Maybe Text),
    _gdrprsFolderConfigurations ::
      !( Maybe
           [FolderConfiguration]
       ),
    _gdrprsDescription ::
      !(Maybe Text),
    _gdrprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDefaultRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrprsName' - The retention policy name.
--
-- * 'gdrprsId' - The retention policy ID.
--
-- * 'gdrprsFolderConfigurations' - The retention policy folder configurations.
--
-- * 'gdrprsDescription' - The retention policy description.
--
-- * 'gdrprsResponseStatus' - -- | The response status code.
getDefaultRetentionPolicyResponse ::
  -- | 'gdrprsResponseStatus'
  Int ->
  GetDefaultRetentionPolicyResponse
getDefaultRetentionPolicyResponse pResponseStatus_ =
  GetDefaultRetentionPolicyResponse'
    { _gdrprsName = Nothing,
      _gdrprsId = Nothing,
      _gdrprsFolderConfigurations = Nothing,
      _gdrprsDescription = Nothing,
      _gdrprsResponseStatus = pResponseStatus_
    }

-- | The retention policy name.
gdrprsName :: Lens' GetDefaultRetentionPolicyResponse (Maybe Text)
gdrprsName = lens _gdrprsName (\s a -> s {_gdrprsName = a})

-- | The retention policy ID.
gdrprsId :: Lens' GetDefaultRetentionPolicyResponse (Maybe Text)
gdrprsId = lens _gdrprsId (\s a -> s {_gdrprsId = a})

-- | The retention policy folder configurations.
gdrprsFolderConfigurations :: Lens' GetDefaultRetentionPolicyResponse [FolderConfiguration]
gdrprsFolderConfigurations = lens _gdrprsFolderConfigurations (\s a -> s {_gdrprsFolderConfigurations = a}) . _Default . _Coerce

-- | The retention policy description.
gdrprsDescription :: Lens' GetDefaultRetentionPolicyResponse (Maybe Text)
gdrprsDescription = lens _gdrprsDescription (\s a -> s {_gdrprsDescription = a})

-- | -- | The response status code.
gdrprsResponseStatus :: Lens' GetDefaultRetentionPolicyResponse Int
gdrprsResponseStatus = lens _gdrprsResponseStatus (\s a -> s {_gdrprsResponseStatus = a})

instance NFData GetDefaultRetentionPolicyResponse
