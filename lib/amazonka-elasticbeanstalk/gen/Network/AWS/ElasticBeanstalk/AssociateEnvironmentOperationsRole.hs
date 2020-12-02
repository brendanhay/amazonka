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
-- Module      : Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or change the operations role used by an environment. After this call is made, Elastic Beanstalk uses the associated operations role for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
  ( -- * Creating a Request
    associateEnvironmentOperationsRole,
    AssociateEnvironmentOperationsRole,

    -- * Request Lenses
    aeorEnvironmentName,
    aeorOperationsRole,

    -- * Destructuring the Response
    associateEnvironmentOperationsRoleResponse,
    AssociateEnvironmentOperationsRoleResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to add or change the operations role used by an environment.
--
--
--
-- /See:/ 'associateEnvironmentOperationsRole' smart constructor.
data AssociateEnvironmentOperationsRole = AssociateEnvironmentOperationsRole'
  { _aeorEnvironmentName ::
      !Text,
    _aeorOperationsRole ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateEnvironmentOperationsRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeorEnvironmentName' - The name of the environment to which to set the operations role.
--
-- * 'aeorOperationsRole' - The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
associateEnvironmentOperationsRole ::
  -- | 'aeorEnvironmentName'
  Text ->
  -- | 'aeorOperationsRole'
  Text ->
  AssociateEnvironmentOperationsRole
associateEnvironmentOperationsRole
  pEnvironmentName_
  pOperationsRole_ =
    AssociateEnvironmentOperationsRole'
      { _aeorEnvironmentName =
          pEnvironmentName_,
        _aeorOperationsRole = pOperationsRole_
      }

-- | The name of the environment to which to set the operations role.
aeorEnvironmentName :: Lens' AssociateEnvironmentOperationsRole Text
aeorEnvironmentName = lens _aeorEnvironmentName (\s a -> s {_aeorEnvironmentName = a})

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
aeorOperationsRole :: Lens' AssociateEnvironmentOperationsRole Text
aeorOperationsRole = lens _aeorOperationsRole (\s a -> s {_aeorOperationsRole = a})

instance AWSRequest AssociateEnvironmentOperationsRole where
  type
    Rs AssociateEnvironmentOperationsRole =
      AssociateEnvironmentOperationsRoleResponse
  request = postQuery elasticBeanstalk
  response = receiveNull AssociateEnvironmentOperationsRoleResponse'

instance Hashable AssociateEnvironmentOperationsRole

instance NFData AssociateEnvironmentOperationsRole

instance ToHeaders AssociateEnvironmentOperationsRole where
  toHeaders = const mempty

instance ToPath AssociateEnvironmentOperationsRole where
  toPath = const "/"

instance ToQuery AssociateEnvironmentOperationsRole where
  toQuery AssociateEnvironmentOperationsRole' {..} =
    mconcat
      [ "Action" =: ("AssociateEnvironmentOperationsRole" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "EnvironmentName" =: _aeorEnvironmentName,
        "OperationsRole" =: _aeorOperationsRole
      ]

-- | /See:/ 'associateEnvironmentOperationsRoleResponse' smart constructor.
data AssociateEnvironmentOperationsRoleResponse = AssociateEnvironmentOperationsRoleResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateEnvironmentOperationsRoleResponse' with the minimum fields required to make a request.
associateEnvironmentOperationsRoleResponse ::
  AssociateEnvironmentOperationsRoleResponse
associateEnvironmentOperationsRoleResponse =
  AssociateEnvironmentOperationsRoleResponse'

instance NFData AssociateEnvironmentOperationsRoleResponse
