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
-- Module      : Network.AWS.WorkMail.DeleteOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon WorkMail organization and all underlying AWS resources managed by Amazon WorkMail as part of the organization. You can choose whether to delete the associated directory. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/remove_organization.html Removing an organization> in the /Amazon WorkMail Administrator Guide/ .
module Network.AWS.WorkMail.DeleteOrganization
  ( -- * Creating a Request
    deleteOrganization,
    DeleteOrganization,

    -- * Request Lenses
    doClientToken,
    doOrganizationId,
    doDeleteDirectory,

    -- * Destructuring the Response
    deleteOrganizationResponse,
    DeleteOrganizationResponse,

    -- * Response Lenses
    delrsState,
    delrsOrganizationId,
    delrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'deleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  { _doClientToken ::
      !(Maybe Text),
    _doOrganizationId :: !Text,
    _doDeleteDirectory :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doClientToken' - The idempotency token associated with the request.
--
-- * 'doOrganizationId' - The organization ID.
--
-- * 'doDeleteDirectory' - If true, deletes the AWS Directory Service directory associated with the organization.
deleteOrganization ::
  -- | 'doOrganizationId'
  Text ->
  -- | 'doDeleteDirectory'
  Bool ->
  DeleteOrganization
deleteOrganization pOrganizationId_ pDeleteDirectory_ =
  DeleteOrganization'
    { _doClientToken = Nothing,
      _doOrganizationId = pOrganizationId_,
      _doDeleteDirectory = pDeleteDirectory_
    }

-- | The idempotency token associated with the request.
doClientToken :: Lens' DeleteOrganization (Maybe Text)
doClientToken = lens _doClientToken (\s a -> s {_doClientToken = a})

-- | The organization ID.
doOrganizationId :: Lens' DeleteOrganization Text
doOrganizationId = lens _doOrganizationId (\s a -> s {_doOrganizationId = a})

-- | If true, deletes the AWS Directory Service directory associated with the organization.
doDeleteDirectory :: Lens' DeleteOrganization Bool
doDeleteDirectory = lens _doDeleteDirectory (\s a -> s {_doDeleteDirectory = a})

instance AWSRequest DeleteOrganization where
  type Rs DeleteOrganization = DeleteOrganizationResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          DeleteOrganizationResponse'
            <$> (x .?> "State")
            <*> (x .?> "OrganizationId")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteOrganization

instance NFData DeleteOrganization

instance ToHeaders DeleteOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.DeleteOrganization" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteOrganization where
  toJSON DeleteOrganization' {..} =
    object
      ( catMaybes
          [ ("ClientToken" .=) <$> _doClientToken,
            Just ("OrganizationId" .= _doOrganizationId),
            Just ("DeleteDirectory" .= _doDeleteDirectory)
          ]
      )

instance ToPath DeleteOrganization where
  toPath = const "/"

instance ToQuery DeleteOrganization where
  toQuery = const mempty

-- | /See:/ 'deleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  { _delrsState ::
      !(Maybe Text),
    _delrsOrganizationId :: !(Maybe Text),
    _delrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsState' - The state of the organization.
--
-- * 'delrsOrganizationId' - The organization ID.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteOrganizationResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteOrganizationResponse
deleteOrganizationResponse pResponseStatus_ =
  DeleteOrganizationResponse'
    { _delrsState = Nothing,
      _delrsOrganizationId = Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | The state of the organization.
delrsState :: Lens' DeleteOrganizationResponse (Maybe Text)
delrsState = lens _delrsState (\s a -> s {_delrsState = a})

-- | The organization ID.
delrsOrganizationId :: Lens' DeleteOrganizationResponse (Maybe Text)
delrsOrganizationId = lens _delrsOrganizationId (\s a -> s {_delrsOrganizationId = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteOrganizationResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteOrganizationResponse
