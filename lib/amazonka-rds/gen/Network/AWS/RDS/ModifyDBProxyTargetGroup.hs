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
-- Module      : Network.AWS.RDS.ModifyDBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of a @DBProxyTargetGroup@ .
module Network.AWS.RDS.ModifyDBProxyTargetGroup
  ( -- * Creating a Request
    modifyDBProxyTargetGroup,
    ModifyDBProxyTargetGroup,

    -- * Request Lenses
    mdptgConnectionPoolConfig,
    mdptgNewName,
    mdptgTargetGroupName,
    mdptgDBProxyName,

    -- * Destructuring the Response
    modifyDBProxyTargetGroupResponse,
    ModifyDBProxyTargetGroupResponse,

    -- * Response Lenses
    mdptgrsDBProxyTargetGroup,
    mdptgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyDBProxyTargetGroup' smart constructor.
data ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroup'
  { _mdptgConnectionPoolConfig ::
      !(Maybe ConnectionPoolConfiguration),
    _mdptgNewName :: !(Maybe Text),
    _mdptgTargetGroupName :: !Text,
    _mdptgDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBProxyTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdptgConnectionPoolConfig' - The settings that determine the size and behavior of the connection pool for the target group.
--
-- * 'mdptgNewName' - The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- * 'mdptgTargetGroupName' - The name of the new target group to assign to the proxy.
--
-- * 'mdptgDBProxyName' - The name of the new proxy to which to assign the target group.
modifyDBProxyTargetGroup ::
  -- | 'mdptgTargetGroupName'
  Text ->
  -- | 'mdptgDBProxyName'
  Text ->
  ModifyDBProxyTargetGroup
modifyDBProxyTargetGroup pTargetGroupName_ pDBProxyName_ =
  ModifyDBProxyTargetGroup'
    { _mdptgConnectionPoolConfig = Nothing,
      _mdptgNewName = Nothing,
      _mdptgTargetGroupName = pTargetGroupName_,
      _mdptgDBProxyName = pDBProxyName_
    }

-- | The settings that determine the size and behavior of the connection pool for the target group.
mdptgConnectionPoolConfig :: Lens' ModifyDBProxyTargetGroup (Maybe ConnectionPoolConfiguration)
mdptgConnectionPoolConfig = lens _mdptgConnectionPoolConfig (\s a -> s {_mdptgConnectionPoolConfig = a})

-- | The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
mdptgNewName :: Lens' ModifyDBProxyTargetGroup (Maybe Text)
mdptgNewName = lens _mdptgNewName (\s a -> s {_mdptgNewName = a})

-- | The name of the new target group to assign to the proxy.
mdptgTargetGroupName :: Lens' ModifyDBProxyTargetGroup Text
mdptgTargetGroupName = lens _mdptgTargetGroupName (\s a -> s {_mdptgTargetGroupName = a})

-- | The name of the new proxy to which to assign the target group.
mdptgDBProxyName :: Lens' ModifyDBProxyTargetGroup Text
mdptgDBProxyName = lens _mdptgDBProxyName (\s a -> s {_mdptgDBProxyName = a})

instance AWSRequest ModifyDBProxyTargetGroup where
  type Rs ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroupResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "ModifyDBProxyTargetGroupResult"
      ( \s h x ->
          ModifyDBProxyTargetGroupResponse'
            <$> (x .@? "DBProxyTargetGroup") <*> (pure (fromEnum s))
      )

instance Hashable ModifyDBProxyTargetGroup

instance NFData ModifyDBProxyTargetGroup

instance ToHeaders ModifyDBProxyTargetGroup where
  toHeaders = const mempty

instance ToPath ModifyDBProxyTargetGroup where
  toPath = const "/"

instance ToQuery ModifyDBProxyTargetGroup where
  toQuery ModifyDBProxyTargetGroup' {..} =
    mconcat
      [ "Action" =: ("ModifyDBProxyTargetGroup" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "ConnectionPoolConfig" =: _mdptgConnectionPoolConfig,
        "NewName" =: _mdptgNewName,
        "TargetGroupName" =: _mdptgTargetGroupName,
        "DBProxyName" =: _mdptgDBProxyName
      ]

-- | /See:/ 'modifyDBProxyTargetGroupResponse' smart constructor.
data ModifyDBProxyTargetGroupResponse = ModifyDBProxyTargetGroupResponse'
  { _mdptgrsDBProxyTargetGroup ::
      !( Maybe
           DBProxyTargetGroup
       ),
    _mdptgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBProxyTargetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdptgrsDBProxyTargetGroup' - The settings of the modified @DBProxyTarget@ .
--
-- * 'mdptgrsResponseStatus' - -- | The response status code.
modifyDBProxyTargetGroupResponse ::
  -- | 'mdptgrsResponseStatus'
  Int ->
  ModifyDBProxyTargetGroupResponse
modifyDBProxyTargetGroupResponse pResponseStatus_ =
  ModifyDBProxyTargetGroupResponse'
    { _mdptgrsDBProxyTargetGroup =
        Nothing,
      _mdptgrsResponseStatus = pResponseStatus_
    }

-- | The settings of the modified @DBProxyTarget@ .
mdptgrsDBProxyTargetGroup :: Lens' ModifyDBProxyTargetGroupResponse (Maybe DBProxyTargetGroup)
mdptgrsDBProxyTargetGroup = lens _mdptgrsDBProxyTargetGroup (\s a -> s {_mdptgrsDBProxyTargetGroup = a})

-- | -- | The response status code.
mdptgrsResponseStatus :: Lens' ModifyDBProxyTargetGroupResponse Int
mdptgrsResponseStatus = lens _mdptgrsResponseStatus (\s a -> s {_mdptgrsResponseStatus = a})

instance NFData ModifyDBProxyTargetGroupResponse
