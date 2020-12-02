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
-- Module      : Network.AWS.RDS.DeregisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the association between one or more @DBProxyTarget@ data structures and a @DBProxyTargetGroup@ .
module Network.AWS.RDS.DeregisterDBProxyTargets
  ( -- * Creating a Request
    deregisterDBProxyTargets,
    DeregisterDBProxyTargets,

    -- * Request Lenses
    ddbptDBClusterIdentifiers,
    ddbptDBInstanceIdentifiers,
    ddbptTargetGroupName,
    ddbptDBProxyName,

    -- * Destructuring the Response
    deregisterDBProxyTargetsResponse,
    DeregisterDBProxyTargetsResponse,

    -- * Response Lenses
    ddptrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
  { _ddbptDBClusterIdentifiers ::
      !(Maybe [Text]),
    _ddbptDBInstanceIdentifiers ::
      !(Maybe [Text]),
    _ddbptTargetGroupName :: !(Maybe Text),
    _ddbptDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterDBProxyTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbptDBClusterIdentifiers' - One or more DB cluster identifiers.
--
-- * 'ddbptDBInstanceIdentifiers' - One or more DB instance identifiers.
--
-- * 'ddbptTargetGroupName' - The identifier of the @DBProxyTargetGroup@ .
--
-- * 'ddbptDBProxyName' - The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
deregisterDBProxyTargets ::
  -- | 'ddbptDBProxyName'
  Text ->
  DeregisterDBProxyTargets
deregisterDBProxyTargets pDBProxyName_ =
  DeregisterDBProxyTargets'
    { _ddbptDBClusterIdentifiers = Nothing,
      _ddbptDBInstanceIdentifiers = Nothing,
      _ddbptTargetGroupName = Nothing,
      _ddbptDBProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
ddbptDBClusterIdentifiers :: Lens' DeregisterDBProxyTargets [Text]
ddbptDBClusterIdentifiers = lens _ddbptDBClusterIdentifiers (\s a -> s {_ddbptDBClusterIdentifiers = a}) . _Default . _Coerce

-- | One or more DB instance identifiers.
ddbptDBInstanceIdentifiers :: Lens' DeregisterDBProxyTargets [Text]
ddbptDBInstanceIdentifiers = lens _ddbptDBInstanceIdentifiers (\s a -> s {_ddbptDBInstanceIdentifiers = a}) . _Default . _Coerce

-- | The identifier of the @DBProxyTargetGroup@ .
ddbptTargetGroupName :: Lens' DeregisterDBProxyTargets (Maybe Text)
ddbptTargetGroupName = lens _ddbptTargetGroupName (\s a -> s {_ddbptTargetGroupName = a})

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
ddbptDBProxyName :: Lens' DeregisterDBProxyTargets Text
ddbptDBProxyName = lens _ddbptDBProxyName (\s a -> s {_ddbptDBProxyName = a})

instance AWSRequest DeregisterDBProxyTargets where
  type Rs DeregisterDBProxyTargets = DeregisterDBProxyTargetsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DeregisterDBProxyTargetsResult"
      ( \s h x ->
          DeregisterDBProxyTargetsResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeregisterDBProxyTargets

instance NFData DeregisterDBProxyTargets

instance ToHeaders DeregisterDBProxyTargets where
  toHeaders = const mempty

instance ToPath DeregisterDBProxyTargets where
  toPath = const "/"

instance ToQuery DeregisterDBProxyTargets where
  toQuery DeregisterDBProxyTargets' {..} =
    mconcat
      [ "Action" =: ("DeregisterDBProxyTargets" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBClusterIdentifiers"
          =: toQuery (toQueryList "member" <$> _ddbptDBClusterIdentifiers),
        "DBInstanceIdentifiers"
          =: toQuery (toQueryList "member" <$> _ddbptDBInstanceIdentifiers),
        "TargetGroupName" =: _ddbptTargetGroupName,
        "DBProxyName" =: _ddbptDBProxyName
      ]

-- | /See:/ 'deregisterDBProxyTargetsResponse' smart constructor.
newtype DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { _ddptrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddptrsResponseStatus' - -- | The response status code.
deregisterDBProxyTargetsResponse ::
  -- | 'ddptrsResponseStatus'
  Int ->
  DeregisterDBProxyTargetsResponse
deregisterDBProxyTargetsResponse pResponseStatus_ =
  DeregisterDBProxyTargetsResponse'
    { _ddptrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddptrsResponseStatus :: Lens' DeregisterDBProxyTargetsResponse Int
ddptrsResponseStatus = lens _ddptrsResponseStatus (\s a -> s {_ddptrsResponseStatus = a})

instance NFData DeregisterDBProxyTargetsResponse
