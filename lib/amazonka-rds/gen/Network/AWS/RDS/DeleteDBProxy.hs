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
-- Module      : Network.AWS.RDS.DeleteDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing proxy.
module Network.AWS.RDS.DeleteDBProxy
  ( -- * Creating a Request
    deleteDBProxy,
    DeleteDBProxy,

    -- * Request Lenses
    ddpDBProxyName,

    -- * Destructuring the Response
    deleteDBProxyResponse,
    DeleteDBProxyResponse,

    -- * Response Lenses
    ddbprsDBProxy,
    ddbprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDBProxy' smart constructor.
newtype DeleteDBProxy = DeleteDBProxy' {_ddpDBProxyName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDBProxy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpDBProxyName' - The name of the DB proxy to delete.
deleteDBProxy ::
  -- | 'ddpDBProxyName'
  Text ->
  DeleteDBProxy
deleteDBProxy pDBProxyName_ =
  DeleteDBProxy' {_ddpDBProxyName = pDBProxyName_}

-- | The name of the DB proxy to delete.
ddpDBProxyName :: Lens' DeleteDBProxy Text
ddpDBProxyName = lens _ddpDBProxyName (\s a -> s {_ddpDBProxyName = a})

instance AWSRequest DeleteDBProxy where
  type Rs DeleteDBProxy = DeleteDBProxyResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DeleteDBProxyResult"
      ( \s h x ->
          DeleteDBProxyResponse'
            <$> (x .@? "DBProxy") <*> (pure (fromEnum s))
      )

instance Hashable DeleteDBProxy

instance NFData DeleteDBProxy

instance ToHeaders DeleteDBProxy where
  toHeaders = const mempty

instance ToPath DeleteDBProxy where
  toPath = const "/"

instance ToQuery DeleteDBProxy where
  toQuery DeleteDBProxy' {..} =
    mconcat
      [ "Action" =: ("DeleteDBProxy" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBProxyName" =: _ddpDBProxyName
      ]

-- | /See:/ 'deleteDBProxyResponse' smart constructor.
data DeleteDBProxyResponse = DeleteDBProxyResponse'
  { _ddbprsDBProxy ::
      !(Maybe DBProxy),
    _ddbprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDBProxyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbprsDBProxy' - The data structure representing the details of the DB proxy that you delete.
--
-- * 'ddbprsResponseStatus' - -- | The response status code.
deleteDBProxyResponse ::
  -- | 'ddbprsResponseStatus'
  Int ->
  DeleteDBProxyResponse
deleteDBProxyResponse pResponseStatus_ =
  DeleteDBProxyResponse'
    { _ddbprsDBProxy = Nothing,
      _ddbprsResponseStatus = pResponseStatus_
    }

-- | The data structure representing the details of the DB proxy that you delete.
ddbprsDBProxy :: Lens' DeleteDBProxyResponse (Maybe DBProxy)
ddbprsDBProxy = lens _ddbprsDBProxy (\s a -> s {_ddbprsDBProxy = a})

-- | -- | The response status code.
ddbprsResponseStatus :: Lens' DeleteDBProxyResponse Int
ddbprsResponseStatus = lens _ddbprsResponseStatus (\s a -> s {_ddbprsResponseStatus = a})

instance NFData DeleteDBProxyResponse
