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
-- Module      : Network.AWS.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the user, group, or resource is enabled and fails if the user, group, or resource is deleted. This operation results in the accumulation of costs. For more information, see <https://aws.amazon.com/workmail/pricing Pricing> . The equivalent console functionality for this operation is /Enable/ .
--
--
-- Users can either be created by calling the 'CreateUser' API operation or they can be synchronized from your directory. For more information, see 'DeregisterFromWorkMail' .
module Network.AWS.WorkMail.RegisterToWorkMail
  ( -- * Creating a Request
    registerToWorkMail,
    RegisterToWorkMail,

    -- * Request Lenses
    rtwmOrganizationId,
    rtwmEntityId,
    rtwmEmail,

    -- * Destructuring the Response
    registerToWorkMailResponse,
    RegisterToWorkMailResponse,

    -- * Response Lenses
    rtwmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'registerToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { _rtwmOrganizationId ::
      !Text,
    _rtwmEntityId :: !Text,
    _rtwmEmail :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterToWorkMail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmOrganizationId' - The identifier for the organization under which the user, group, or resource exists.
--
-- * 'rtwmEntityId' - The identifier for the user, group, or resource to be updated.
--
-- * 'rtwmEmail' - The email for the user, group, or resource to be updated.
registerToWorkMail ::
  -- | 'rtwmOrganizationId'
  Text ->
  -- | 'rtwmEntityId'
  Text ->
  -- | 'rtwmEmail'
  Text ->
  RegisterToWorkMail
registerToWorkMail pOrganizationId_ pEntityId_ pEmail_ =
  RegisterToWorkMail'
    { _rtwmOrganizationId = pOrganizationId_,
      _rtwmEntityId = pEntityId_,
      _rtwmEmail = pEmail_
    }

-- | The identifier for the organization under which the user, group, or resource exists.
rtwmOrganizationId :: Lens' RegisterToWorkMail Text
rtwmOrganizationId = lens _rtwmOrganizationId (\s a -> s {_rtwmOrganizationId = a})

-- | The identifier for the user, group, or resource to be updated.
rtwmEntityId :: Lens' RegisterToWorkMail Text
rtwmEntityId = lens _rtwmEntityId (\s a -> s {_rtwmEntityId = a})

-- | The email for the user, group, or resource to be updated.
rtwmEmail :: Lens' RegisterToWorkMail Text
rtwmEmail = lens _rtwmEmail (\s a -> s {_rtwmEmail = a})

instance AWSRequest RegisterToWorkMail where
  type Rs RegisterToWorkMail = RegisterToWorkMailResponse
  request = postJSON workMail
  response =
    receiveEmpty
      (\s h x -> RegisterToWorkMailResponse' <$> (pure (fromEnum s)))

instance Hashable RegisterToWorkMail

instance NFData RegisterToWorkMail

instance ToHeaders RegisterToWorkMail where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.RegisterToWorkMail" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterToWorkMail where
  toJSON RegisterToWorkMail' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _rtwmOrganizationId),
            Just ("EntityId" .= _rtwmEntityId),
            Just ("Email" .= _rtwmEmail)
          ]
      )

instance ToPath RegisterToWorkMail where
  toPath = const "/"

instance ToQuery RegisterToWorkMail where
  toQuery = const mempty

-- | /See:/ 'registerToWorkMailResponse' smart constructor.
newtype RegisterToWorkMailResponse = RegisterToWorkMailResponse'
  { _rtwmrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterToWorkMailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmrsResponseStatus' - -- | The response status code.
registerToWorkMailResponse ::
  -- | 'rtwmrsResponseStatus'
  Int ->
  RegisterToWorkMailResponse
registerToWorkMailResponse pResponseStatus_ =
  RegisterToWorkMailResponse'
    { _rtwmrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
rtwmrsResponseStatus :: Lens' RegisterToWorkMailResponse Int
rtwmrsResponseStatus = lens _rtwmrsResponseStatus (\s a -> s {_rtwmrsResponseStatus = a})

instance NFData RegisterToWorkMailResponse
