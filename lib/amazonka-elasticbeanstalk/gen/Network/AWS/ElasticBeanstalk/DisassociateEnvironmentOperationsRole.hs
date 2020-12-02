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
-- Module      : Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate the operations role from an environment. After this call is made, Elastic Beanstalk uses the caller's permissions for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
  ( -- * Creating a Request
    disassociateEnvironmentOperationsRole,
    DisassociateEnvironmentOperationsRole,

    -- * Request Lenses
    deorEnvironmentName,

    -- * Destructuring the Response
    disassociateEnvironmentOperationsRoleResponse,
    DisassociateEnvironmentOperationsRoleResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to disassociate the operations role from an environment.
--
--
--
-- /See:/ 'disassociateEnvironmentOperationsRole' smart constructor.
newtype DisassociateEnvironmentOperationsRole = DisassociateEnvironmentOperationsRole'
  { _deorEnvironmentName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateEnvironmentOperationsRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deorEnvironmentName' - The name of the environment from which to disassociate the operations role.
disassociateEnvironmentOperationsRole ::
  -- | 'deorEnvironmentName'
  Text ->
  DisassociateEnvironmentOperationsRole
disassociateEnvironmentOperationsRole pEnvironmentName_ =
  DisassociateEnvironmentOperationsRole'
    { _deorEnvironmentName =
        pEnvironmentName_
    }

-- | The name of the environment from which to disassociate the operations role.
deorEnvironmentName :: Lens' DisassociateEnvironmentOperationsRole Text
deorEnvironmentName = lens _deorEnvironmentName (\s a -> s {_deorEnvironmentName = a})

instance AWSRequest DisassociateEnvironmentOperationsRole where
  type
    Rs DisassociateEnvironmentOperationsRole =
      DisassociateEnvironmentOperationsRoleResponse
  request = postQuery elasticBeanstalk
  response =
    receiveNull DisassociateEnvironmentOperationsRoleResponse'

instance Hashable DisassociateEnvironmentOperationsRole

instance NFData DisassociateEnvironmentOperationsRole

instance ToHeaders DisassociateEnvironmentOperationsRole where
  toHeaders = const mempty

instance ToPath DisassociateEnvironmentOperationsRole where
  toPath = const "/"

instance ToQuery DisassociateEnvironmentOperationsRole where
  toQuery DisassociateEnvironmentOperationsRole' {..} =
    mconcat
      [ "Action"
          =: ("DisassociateEnvironmentOperationsRole" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "EnvironmentName" =: _deorEnvironmentName
      ]

-- | /See:/ 'disassociateEnvironmentOperationsRoleResponse' smart constructor.
data DisassociateEnvironmentOperationsRoleResponse = DisassociateEnvironmentOperationsRoleResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisassociateEnvironmentOperationsRoleResponse' with the minimum fields required to make a request.
disassociateEnvironmentOperationsRoleResponse ::
  DisassociateEnvironmentOperationsRoleResponse
disassociateEnvironmentOperationsRoleResponse =
  DisassociateEnvironmentOperationsRoleResponse'

instance NFData DisassociateEnvironmentOperationsRoleResponse
