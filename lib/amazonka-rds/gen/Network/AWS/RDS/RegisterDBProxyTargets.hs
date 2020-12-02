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
-- Module      : Network.AWS.RDS.RegisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate one or more @DBProxyTarget@ data structures with a @DBProxyTargetGroup@ .
module Network.AWS.RDS.RegisterDBProxyTargets
  ( -- * Creating a Request
    registerDBProxyTargets,
    RegisterDBProxyTargets,

    -- * Request Lenses
    rdptDBClusterIdentifiers,
    rdptDBInstanceIdentifiers,
    rdptTargetGroupName,
    rdptDBProxyName,

    -- * Destructuring the Response
    registerDBProxyTargetsResponse,
    RegisterDBProxyTargetsResponse,

    -- * Response Lenses
    rdptrsDBProxyTargets,
    rdptrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
  { _rdptDBClusterIdentifiers ::
      !(Maybe [Text]),
    _rdptDBInstanceIdentifiers :: !(Maybe [Text]),
    _rdptTargetGroupName :: !(Maybe Text),
    _rdptDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterDBProxyTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdptDBClusterIdentifiers' - One or more DB cluster identifiers.
--
-- * 'rdptDBInstanceIdentifiers' - One or more DB instance identifiers.
--
-- * 'rdptTargetGroupName' - The identifier of the @DBProxyTargetGroup@ .
--
-- * 'rdptDBProxyName' - The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
registerDBProxyTargets ::
  -- | 'rdptDBProxyName'
  Text ->
  RegisterDBProxyTargets
registerDBProxyTargets pDBProxyName_ =
  RegisterDBProxyTargets'
    { _rdptDBClusterIdentifiers = Nothing,
      _rdptDBInstanceIdentifiers = Nothing,
      _rdptTargetGroupName = Nothing,
      _rdptDBProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
rdptDBClusterIdentifiers :: Lens' RegisterDBProxyTargets [Text]
rdptDBClusterIdentifiers = lens _rdptDBClusterIdentifiers (\s a -> s {_rdptDBClusterIdentifiers = a}) . _Default . _Coerce

-- | One or more DB instance identifiers.
rdptDBInstanceIdentifiers :: Lens' RegisterDBProxyTargets [Text]
rdptDBInstanceIdentifiers = lens _rdptDBInstanceIdentifiers (\s a -> s {_rdptDBInstanceIdentifiers = a}) . _Default . _Coerce

-- | The identifier of the @DBProxyTargetGroup@ .
rdptTargetGroupName :: Lens' RegisterDBProxyTargets (Maybe Text)
rdptTargetGroupName = lens _rdptTargetGroupName (\s a -> s {_rdptTargetGroupName = a})

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
rdptDBProxyName :: Lens' RegisterDBProxyTargets Text
rdptDBProxyName = lens _rdptDBProxyName (\s a -> s {_rdptDBProxyName = a})

instance AWSRequest RegisterDBProxyTargets where
  type Rs RegisterDBProxyTargets = RegisterDBProxyTargetsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            <$> (x .@? "DBProxyTargets" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable RegisterDBProxyTargets

instance NFData RegisterDBProxyTargets

instance ToHeaders RegisterDBProxyTargets where
  toHeaders = const mempty

instance ToPath RegisterDBProxyTargets where
  toPath = const "/"

instance ToQuery RegisterDBProxyTargets where
  toQuery RegisterDBProxyTargets' {..} =
    mconcat
      [ "Action" =: ("RegisterDBProxyTargets" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBClusterIdentifiers"
          =: toQuery (toQueryList "member" <$> _rdptDBClusterIdentifiers),
        "DBInstanceIdentifiers"
          =: toQuery (toQueryList "member" <$> _rdptDBInstanceIdentifiers),
        "TargetGroupName" =: _rdptTargetGroupName,
        "DBProxyName" =: _rdptDBProxyName
      ]

-- | /See:/ 'registerDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { _rdptrsDBProxyTargets ::
      !(Maybe [DBProxyTarget]),
    _rdptrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdptrsDBProxyTargets' - One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
--
-- * 'rdptrsResponseStatus' - -- | The response status code.
registerDBProxyTargetsResponse ::
  -- | 'rdptrsResponseStatus'
  Int ->
  RegisterDBProxyTargetsResponse
registerDBProxyTargetsResponse pResponseStatus_ =
  RegisterDBProxyTargetsResponse'
    { _rdptrsDBProxyTargets = Nothing,
      _rdptrsResponseStatus = pResponseStatus_
    }

-- | One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
rdptrsDBProxyTargets :: Lens' RegisterDBProxyTargetsResponse [DBProxyTarget]
rdptrsDBProxyTargets = lens _rdptrsDBProxyTargets (\s a -> s {_rdptrsDBProxyTargets = a}) . _Default . _Coerce

-- | -- | The response status code.
rdptrsResponseStatus :: Lens' RegisterDBProxyTargetsResponse Int
rdptrsResponseStatus = lens _rdptrsResponseStatus (\s a -> s {_rdptrsResponseStatus = a})

instance NFData RegisterDBProxyTargetsResponse
