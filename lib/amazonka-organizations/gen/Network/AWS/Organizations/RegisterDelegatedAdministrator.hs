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
-- Module      : Network.AWS.Organizations.RegisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified member account to administer the Organizations features of the specified AWS service. It grants read-only access to AWS Organizations service data. The account still requires IAM permissions to access and administer the AWS service.
--
--
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.RegisterDelegatedAdministrator
  ( -- * Creating a Request
    registerDelegatedAdministrator,
    RegisterDelegatedAdministrator,

    -- * Request Lenses
    rdaAccountId,
    rdaServicePrincipal,

    -- * Destructuring the Response
    registerDelegatedAdministratorResponse,
    RegisterDelegatedAdministratorResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { _rdaAccountId ::
      !Text,
    _rdaServicePrincipal :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterDelegatedAdministrator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdaAccountId' - The account ID number of the member account in the organization to register as a delegated administrator.
--
-- * 'rdaServicePrincipal' - The service principal of the AWS service for which you want to make the member account a delegated administrator.
registerDelegatedAdministrator ::
  -- | 'rdaAccountId'
  Text ->
  -- | 'rdaServicePrincipal'
  Text ->
  RegisterDelegatedAdministrator
registerDelegatedAdministrator pAccountId_ pServicePrincipal_ =
  RegisterDelegatedAdministrator'
    { _rdaAccountId = pAccountId_,
      _rdaServicePrincipal = pServicePrincipal_
    }

-- | The account ID number of the member account in the organization to register as a delegated administrator.
rdaAccountId :: Lens' RegisterDelegatedAdministrator Text
rdaAccountId = lens _rdaAccountId (\s a -> s {_rdaAccountId = a})

-- | The service principal of the AWS service for which you want to make the member account a delegated administrator.
rdaServicePrincipal :: Lens' RegisterDelegatedAdministrator Text
rdaServicePrincipal = lens _rdaServicePrincipal (\s a -> s {_rdaServicePrincipal = a})

instance AWSRequest RegisterDelegatedAdministrator where
  type
    Rs RegisterDelegatedAdministrator =
      RegisterDelegatedAdministratorResponse
  request = postJSON organizations
  response = receiveNull RegisterDelegatedAdministratorResponse'

instance Hashable RegisterDelegatedAdministrator

instance NFData RegisterDelegatedAdministrator

instance ToHeaders RegisterDelegatedAdministrator where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrganizationsV20161128.RegisterDelegatedAdministrator" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterDelegatedAdministrator where
  toJSON RegisterDelegatedAdministrator' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _rdaAccountId),
            Just ("ServicePrincipal" .= _rdaServicePrincipal)
          ]
      )

instance ToPath RegisterDelegatedAdministrator where
  toPath = const "/"

instance ToQuery RegisterDelegatedAdministrator where
  toQuery = const mempty

-- | /See:/ 'registerDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterDelegatedAdministratorResponse' with the minimum fields required to make a request.
registerDelegatedAdministratorResponse ::
  RegisterDelegatedAdministratorResponse
registerDelegatedAdministratorResponse =
  RegisterDelegatedAdministratorResponse'

instance NFData RegisterDelegatedAdministratorResponse
