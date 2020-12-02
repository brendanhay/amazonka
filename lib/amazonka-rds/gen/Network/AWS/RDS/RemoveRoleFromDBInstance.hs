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
-- Module      : Network.AWS.RDS.RemoveRoleFromDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from a DB instance.
module Network.AWS.RDS.RemoveRoleFromDBInstance
  ( -- * Creating a Request
    removeRoleFromDBInstance,
    RemoveRoleFromDBInstance,

    -- * Request Lenses
    rrfdiDBInstanceIdentifier,
    rrfdiRoleARN,
    rrfdiFeatureName,

    -- * Destructuring the Response
    removeRoleFromDBInstanceResponse,
    RemoveRoleFromDBInstanceResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeRoleFromDBInstance' smart constructor.
data RemoveRoleFromDBInstance = RemoveRoleFromDBInstance'
  { _rrfdiDBInstanceIdentifier ::
      !Text,
    _rrfdiRoleARN :: !Text,
    _rrfdiFeatureName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveRoleFromDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrfdiDBInstanceIdentifier' - The name of the DB instance to disassociate the IAM role from.
--
-- * 'rrfdiRoleARN' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- * 'rrfdiFeatureName' - The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
removeRoleFromDBInstance ::
  -- | 'rrfdiDBInstanceIdentifier'
  Text ->
  -- | 'rrfdiRoleARN'
  Text ->
  -- | 'rrfdiFeatureName'
  Text ->
  RemoveRoleFromDBInstance
removeRoleFromDBInstance
  pDBInstanceIdentifier_
  pRoleARN_
  pFeatureName_ =
    RemoveRoleFromDBInstance'
      { _rrfdiDBInstanceIdentifier =
          pDBInstanceIdentifier_,
        _rrfdiRoleARN = pRoleARN_,
        _rrfdiFeatureName = pFeatureName_
      }

-- | The name of the DB instance to disassociate the IAM role from.
rrfdiDBInstanceIdentifier :: Lens' RemoveRoleFromDBInstance Text
rrfdiDBInstanceIdentifier = lens _rrfdiDBInstanceIdentifier (\s a -> s {_rrfdiDBInstanceIdentifier = a})

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
rrfdiRoleARN :: Lens' RemoveRoleFromDBInstance Text
rrfdiRoleARN = lens _rrfdiRoleARN (\s a -> s {_rrfdiRoleARN = a})

-- | The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
rrfdiFeatureName :: Lens' RemoveRoleFromDBInstance Text
rrfdiFeatureName = lens _rrfdiFeatureName (\s a -> s {_rrfdiFeatureName = a})

instance AWSRequest RemoveRoleFromDBInstance where
  type Rs RemoveRoleFromDBInstance = RemoveRoleFromDBInstanceResponse
  request = postQuery rds
  response = receiveNull RemoveRoleFromDBInstanceResponse'

instance Hashable RemoveRoleFromDBInstance

instance NFData RemoveRoleFromDBInstance

instance ToHeaders RemoveRoleFromDBInstance where
  toHeaders = const mempty

instance ToPath RemoveRoleFromDBInstance where
  toPath = const "/"

instance ToQuery RemoveRoleFromDBInstance where
  toQuery RemoveRoleFromDBInstance' {..} =
    mconcat
      [ "Action" =: ("RemoveRoleFromDBInstance" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBInstanceIdentifier" =: _rrfdiDBInstanceIdentifier,
        "RoleArn" =: _rrfdiRoleARN,
        "FeatureName" =: _rrfdiFeatureName
      ]

-- | /See:/ 'removeRoleFromDBInstanceResponse' smart constructor.
data RemoveRoleFromDBInstanceResponse = RemoveRoleFromDBInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveRoleFromDBInstanceResponse' with the minimum fields required to make a request.
removeRoleFromDBInstanceResponse ::
  RemoveRoleFromDBInstanceResponse
removeRoleFromDBInstanceResponse =
  RemoveRoleFromDBInstanceResponse'

instance NFData RemoveRoleFromDBInstanceResponse
