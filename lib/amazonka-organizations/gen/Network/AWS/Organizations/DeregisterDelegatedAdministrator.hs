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
-- Module      : Network.AWS.Organizations.DeregisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member AWS account as a delegated administrator for the specified AWS service.
--
--
-- /Important:/ Deregistering a delegated administrator can have unintended impacts on the functionality of the enabled AWS service. See the documentation for the enabled service before you deregister a delegated administrator so that you understand any potential impacts.
--
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeregisterDelegatedAdministrator
  ( -- * Creating a Request
    deregisterDelegatedAdministrator,
    DeregisterDelegatedAdministrator,

    -- * Request Lenses
    ddaAccountId,
    ddaServicePrincipal,

    -- * Destructuring the Response
    deregisterDelegatedAdministratorResponse,
    DeregisterDelegatedAdministratorResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterDelegatedAdministrator' smart constructor.
data DeregisterDelegatedAdministrator = DeregisterDelegatedAdministrator'
  { _ddaAccountId ::
      !Text,
    _ddaServicePrincipal ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterDelegatedAdministrator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddaAccountId' - The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
--
-- * 'ddaServicePrincipal' - The service principal name of an AWS service for which the account is a delegated administrator. Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
deregisterDelegatedAdministrator ::
  -- | 'ddaAccountId'
  Text ->
  -- | 'ddaServicePrincipal'
  Text ->
  DeregisterDelegatedAdministrator
deregisterDelegatedAdministrator pAccountId_ pServicePrincipal_ =
  DeregisterDelegatedAdministrator'
    { _ddaAccountId = pAccountId_,
      _ddaServicePrincipal = pServicePrincipal_
    }

-- | The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
ddaAccountId :: Lens' DeregisterDelegatedAdministrator Text
ddaAccountId = lens _ddaAccountId (\s a -> s {_ddaAccountId = a})

-- | The service principal name of an AWS service for which the account is a delegated administrator. Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
ddaServicePrincipal :: Lens' DeregisterDelegatedAdministrator Text
ddaServicePrincipal = lens _ddaServicePrincipal (\s a -> s {_ddaServicePrincipal = a})

instance AWSRequest DeregisterDelegatedAdministrator where
  type
    Rs DeregisterDelegatedAdministrator =
      DeregisterDelegatedAdministratorResponse
  request = postJSON organizations
  response = receiveNull DeregisterDelegatedAdministratorResponse'

instance Hashable DeregisterDelegatedAdministrator

instance NFData DeregisterDelegatedAdministrator

instance ToHeaders DeregisterDelegatedAdministrator where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrganizationsV20161128.DeregisterDelegatedAdministrator" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeregisterDelegatedAdministrator where
  toJSON DeregisterDelegatedAdministrator' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _ddaAccountId),
            Just ("ServicePrincipal" .= _ddaServicePrincipal)
          ]
      )

instance ToPath DeregisterDelegatedAdministrator where
  toPath = const "/"

instance ToQuery DeregisterDelegatedAdministrator where
  toQuery = const mempty

-- | /See:/ 'deregisterDelegatedAdministratorResponse' smart constructor.
data DeregisterDelegatedAdministratorResponse = DeregisterDelegatedAdministratorResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterDelegatedAdministratorResponse' with the minimum fields required to make a request.
deregisterDelegatedAdministratorResponse ::
  DeregisterDelegatedAdministratorResponse
deregisterDelegatedAdministratorResponse =
  DeregisterDelegatedAdministratorResponse'

instance NFData DeregisterDelegatedAdministratorResponse
