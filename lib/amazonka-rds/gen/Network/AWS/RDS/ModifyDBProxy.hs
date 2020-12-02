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
-- Module      : Network.AWS.RDS.ModifyDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings for an existing DB proxy.
module Network.AWS.RDS.ModifyDBProxy
  ( -- * Creating a Request
    modifyDBProxy,
    ModifyDBProxy,

    -- * Request Lenses
    mdpDebugLogging,
    mdpSecurityGroups,
    mdpAuth,
    mdpRequireTLS,
    mdpIdleClientTimeout,
    mdpNewDBProxyName,
    mdpRoleARN,
    mdpDBProxyName,

    -- * Destructuring the Response
    modifyDBProxyResponse,
    ModifyDBProxyResponse,

    -- * Response Lenses
    mdprsDBProxy,
    mdprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { _mdpDebugLogging ::
      !(Maybe Bool),
    _mdpSecurityGroups :: !(Maybe [Text]),
    _mdpAuth :: !(Maybe [UserAuthConfig]),
    _mdpRequireTLS :: !(Maybe Bool),
    _mdpIdleClientTimeout :: !(Maybe Int),
    _mdpNewDBProxyName :: !(Maybe Text),
    _mdpRoleARN :: !(Maybe Text),
    _mdpDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBProxy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdpDebugLogging' - Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- * 'mdpSecurityGroups' - The new list of security groups for the @DBProxy@ .
--
-- * 'mdpAuth' - The new authentication settings for the @DBProxy@ .
--
-- * 'mdpRequireTLS' - Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
--
-- * 'mdpIdleClientTimeout' - The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- * 'mdpNewDBProxyName' - The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- * 'mdpRoleARN' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- * 'mdpDBProxyName' - The identifier for the @DBProxy@ to modify.
modifyDBProxy ::
  -- | 'mdpDBProxyName'
  Text ->
  ModifyDBProxy
modifyDBProxy pDBProxyName_ =
  ModifyDBProxy'
    { _mdpDebugLogging = Nothing,
      _mdpSecurityGroups = Nothing,
      _mdpAuth = Nothing,
      _mdpRequireTLS = Nothing,
      _mdpIdleClientTimeout = Nothing,
      _mdpNewDBProxyName = Nothing,
      _mdpRoleARN = Nothing,
      _mdpDBProxyName = pDBProxyName_
    }

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
mdpDebugLogging :: Lens' ModifyDBProxy (Maybe Bool)
mdpDebugLogging = lens _mdpDebugLogging (\s a -> s {_mdpDebugLogging = a})

-- | The new list of security groups for the @DBProxy@ .
mdpSecurityGroups :: Lens' ModifyDBProxy [Text]
mdpSecurityGroups = lens _mdpSecurityGroups (\s a -> s {_mdpSecurityGroups = a}) . _Default . _Coerce

-- | The new authentication settings for the @DBProxy@ .
mdpAuth :: Lens' ModifyDBProxy [UserAuthConfig]
mdpAuth = lens _mdpAuth (\s a -> s {_mdpAuth = a}) . _Default . _Coerce

-- | Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
mdpRequireTLS :: Lens' ModifyDBProxy (Maybe Bool)
mdpRequireTLS = lens _mdpRequireTLS (\s a -> s {_mdpRequireTLS = a})

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
mdpIdleClientTimeout :: Lens' ModifyDBProxy (Maybe Int)
mdpIdleClientTimeout = lens _mdpIdleClientTimeout (\s a -> s {_mdpIdleClientTimeout = a})

-- | The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
mdpNewDBProxyName :: Lens' ModifyDBProxy (Maybe Text)
mdpNewDBProxyName = lens _mdpNewDBProxyName (\s a -> s {_mdpNewDBProxyName = a})

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
mdpRoleARN :: Lens' ModifyDBProxy (Maybe Text)
mdpRoleARN = lens _mdpRoleARN (\s a -> s {_mdpRoleARN = a})

-- | The identifier for the @DBProxy@ to modify.
mdpDBProxyName :: Lens' ModifyDBProxy Text
mdpDBProxyName = lens _mdpDBProxyName (\s a -> s {_mdpDBProxyName = a})

instance AWSRequest ModifyDBProxy where
  type Rs ModifyDBProxy = ModifyDBProxyResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "ModifyDBProxyResult"
      ( \s h x ->
          ModifyDBProxyResponse'
            <$> (x .@? "DBProxy") <*> (pure (fromEnum s))
      )

instance Hashable ModifyDBProxy

instance NFData ModifyDBProxy

instance ToHeaders ModifyDBProxy where
  toHeaders = const mempty

instance ToPath ModifyDBProxy where
  toPath = const "/"

instance ToQuery ModifyDBProxy where
  toQuery ModifyDBProxy' {..} =
    mconcat
      [ "Action" =: ("ModifyDBProxy" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DebugLogging" =: _mdpDebugLogging,
        "SecurityGroups"
          =: toQuery (toQueryList "member" <$> _mdpSecurityGroups),
        "Auth" =: toQuery (toQueryList "member" <$> _mdpAuth),
        "RequireTLS" =: _mdpRequireTLS,
        "IdleClientTimeout" =: _mdpIdleClientTimeout,
        "NewDBProxyName" =: _mdpNewDBProxyName,
        "RoleArn" =: _mdpRoleARN,
        "DBProxyName" =: _mdpDBProxyName
      ]

-- | /See:/ 'modifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { _mdprsDBProxy ::
      !(Maybe DBProxy),
    _mdprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBProxyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdprsDBProxy' - The @DBProxy@ object representing the new settings for the proxy.
--
-- * 'mdprsResponseStatus' - -- | The response status code.
modifyDBProxyResponse ::
  -- | 'mdprsResponseStatus'
  Int ->
  ModifyDBProxyResponse
modifyDBProxyResponse pResponseStatus_ =
  ModifyDBProxyResponse'
    { _mdprsDBProxy = Nothing,
      _mdprsResponseStatus = pResponseStatus_
    }

-- | The @DBProxy@ object representing the new settings for the proxy.
mdprsDBProxy :: Lens' ModifyDBProxyResponse (Maybe DBProxy)
mdprsDBProxy = lens _mdprsDBProxy (\s a -> s {_mdprsDBProxy = a})

-- | -- | The response status code.
mdprsResponseStatus :: Lens' ModifyDBProxyResponse Int
mdprsResponseStatus = lens _mdprsResponseStatus (\s a -> s {_mdprsResponseStatus = a})

instance NFData ModifyDBProxyResponse
