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
-- Module      : Network.AWS.WorkMail.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy from the specified organization.
module Network.AWS.WorkMail.DeleteRetentionPolicy
  ( -- * Creating a Request
    deleteRetentionPolicy,
    DeleteRetentionPolicy,

    -- * Request Lenses
    drpOrganizationId,
    drpId,

    -- * Destructuring the Response
    deleteRetentionPolicyResponse,
    DeleteRetentionPolicyResponse,

    -- * Response Lenses
    drprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'deleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { _drpOrganizationId ::
      !Text,
    _drpId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpOrganizationId' - The organization ID.
--
-- * 'drpId' - The retention policy ID.
deleteRetentionPolicy ::
  -- | 'drpOrganizationId'
  Text ->
  -- | 'drpId'
  Text ->
  DeleteRetentionPolicy
deleteRetentionPolicy pOrganizationId_ pId_ =
  DeleteRetentionPolicy'
    { _drpOrganizationId = pOrganizationId_,
      _drpId = pId_
    }

-- | The organization ID.
drpOrganizationId :: Lens' DeleteRetentionPolicy Text
drpOrganizationId = lens _drpOrganizationId (\s a -> s {_drpOrganizationId = a})

-- | The retention policy ID.
drpId :: Lens' DeleteRetentionPolicy Text
drpId = lens _drpId (\s a -> s {_drpId = a})

instance AWSRequest DeleteRetentionPolicy where
  type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
  request = postJSON workMail
  response =
    receiveEmpty
      (\s h x -> DeleteRetentionPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteRetentionPolicy

instance NFData DeleteRetentionPolicy

instance ToHeaders DeleteRetentionPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.DeleteRetentionPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _drpOrganizationId),
            Just ("Id" .= _drpId)
          ]
      )

instance ToPath DeleteRetentionPolicy where
  toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
  toQuery = const mempty

-- | /See:/ 'deleteRetentionPolicyResponse' smart constructor.
newtype DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { _drprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRetentionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsResponseStatus' - -- | The response status code.
deleteRetentionPolicyResponse ::
  -- | 'drprsResponseStatus'
  Int ->
  DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse pResponseStatus_ =
  DeleteRetentionPolicyResponse'
    { _drprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drprsResponseStatus :: Lens' DeleteRetentionPolicyResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\s a -> s {_drprsResponseStatus = a})

instance NFData DeleteRetentionPolicyResponse
