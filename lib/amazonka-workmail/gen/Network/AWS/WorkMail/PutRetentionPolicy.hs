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
-- Module      : Network.AWS.WorkMail.PutRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a retention policy to the specified organization.
module Network.AWS.WorkMail.PutRetentionPolicy
  ( -- * Creating a Request
    putRetentionPolicy,
    PutRetentionPolicy,

    -- * Request Lenses
    prpId,
    prpDescription,
    prpOrganizationId,
    prpName,
    prpFolderConfigurations,

    -- * Destructuring the Response
    putRetentionPolicyResponse,
    PutRetentionPolicyResponse,

    -- * Response Lenses
    prprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'putRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { _prpId ::
      !(Maybe Text),
    _prpDescription :: !(Maybe (Sensitive Text)),
    _prpOrganizationId :: !Text,
    _prpName :: !Text,
    _prpFolderConfigurations :: ![FolderConfiguration]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpId' - The retention policy ID.
--
-- * 'prpDescription' - The retention policy description.
--
-- * 'prpOrganizationId' - The organization ID.
--
-- * 'prpName' - The retention policy name.
--
-- * 'prpFolderConfigurations' - The retention policy folder configurations.
putRetentionPolicy ::
  -- | 'prpOrganizationId'
  Text ->
  -- | 'prpName'
  Text ->
  PutRetentionPolicy
putRetentionPolicy pOrganizationId_ pName_ =
  PutRetentionPolicy'
    { _prpId = Nothing,
      _prpDescription = Nothing,
      _prpOrganizationId = pOrganizationId_,
      _prpName = pName_,
      _prpFolderConfigurations = mempty
    }

-- | The retention policy ID.
prpId :: Lens' PutRetentionPolicy (Maybe Text)
prpId = lens _prpId (\s a -> s {_prpId = a})

-- | The retention policy description.
prpDescription :: Lens' PutRetentionPolicy (Maybe Text)
prpDescription = lens _prpDescription (\s a -> s {_prpDescription = a}) . mapping _Sensitive

-- | The organization ID.
prpOrganizationId :: Lens' PutRetentionPolicy Text
prpOrganizationId = lens _prpOrganizationId (\s a -> s {_prpOrganizationId = a})

-- | The retention policy name.
prpName :: Lens' PutRetentionPolicy Text
prpName = lens _prpName (\s a -> s {_prpName = a})

-- | The retention policy folder configurations.
prpFolderConfigurations :: Lens' PutRetentionPolicy [FolderConfiguration]
prpFolderConfigurations = lens _prpFolderConfigurations (\s a -> s {_prpFolderConfigurations = a}) . _Coerce

instance AWSRequest PutRetentionPolicy where
  type Rs PutRetentionPolicy = PutRetentionPolicyResponse
  request = postJSON workMail
  response =
    receiveEmpty
      (\s h x -> PutRetentionPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutRetentionPolicy

instance NFData PutRetentionPolicy

instance ToHeaders PutRetentionPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.PutRetentionPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    object
      ( catMaybes
          [ ("Id" .=) <$> _prpId,
            ("Description" .=) <$> _prpDescription,
            Just ("OrganizationId" .= _prpOrganizationId),
            Just ("Name" .= _prpName),
            Just ("FolderConfigurations" .= _prpFolderConfigurations)
          ]
      )

instance ToPath PutRetentionPolicy where
  toPath = const "/"

instance ToQuery PutRetentionPolicy where
  toQuery = const mempty

-- | /See:/ 'putRetentionPolicyResponse' smart constructor.
newtype PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  { _prprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prprsResponseStatus' - -- | The response status code.
putRetentionPolicyResponse ::
  -- | 'prprsResponseStatus'
  Int ->
  PutRetentionPolicyResponse
putRetentionPolicyResponse pResponseStatus_ =
  PutRetentionPolicyResponse'
    { _prprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
prprsResponseStatus :: Lens' PutRetentionPolicyResponse Int
prprsResponseStatus = lens _prprsResponseStatus (\s a -> s {_prprsResponseStatus = a})

instance NFData PutRetentionPolicyResponse
