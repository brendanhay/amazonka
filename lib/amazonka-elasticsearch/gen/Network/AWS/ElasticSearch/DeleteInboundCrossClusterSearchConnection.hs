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
-- Module      : Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to delete an existing inbound cross-cluster search connection.
module Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    deleteInboundCrossClusterSearchConnection,
    DeleteInboundCrossClusterSearchConnection,

    -- * Request Lenses
    diccscCrossClusterSearchConnectionId,

    -- * Destructuring the Response
    deleteInboundCrossClusterSearchConnectionResponse,
    DeleteInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    diccscirsCrossClusterSearchConnection,
    diccscirsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DeleteInboundCrossClusterSearchConnection' @ operation.
--
--
--
-- /See:/ 'deleteInboundCrossClusterSearchConnection' smart constructor.
newtype DeleteInboundCrossClusterSearchConnection = DeleteInboundCrossClusterSearchConnection'
  { _diccscCrossClusterSearchConnectionId ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diccscCrossClusterSearchConnectionId' - The id of the inbound connection that you want to permanently delete.
deleteInboundCrossClusterSearchConnection ::
  -- | 'diccscCrossClusterSearchConnectionId'
  Text ->
  DeleteInboundCrossClusterSearchConnection
deleteInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteInboundCrossClusterSearchConnection'
      { _diccscCrossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to permanently delete.
diccscCrossClusterSearchConnectionId :: Lens' DeleteInboundCrossClusterSearchConnection Text
diccscCrossClusterSearchConnectionId = lens _diccscCrossClusterSearchConnectionId (\s a -> s {_diccscCrossClusterSearchConnectionId = a})

instance AWSRequest DeleteInboundCrossClusterSearchConnection where
  type
    Rs DeleteInboundCrossClusterSearchConnection =
      DeleteInboundCrossClusterSearchConnectionResponse
  request = delete elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          DeleteInboundCrossClusterSearchConnectionResponse'
            <$> (x .?> "CrossClusterSearchConnection") <*> (pure (fromEnum s))
      )

instance Hashable DeleteInboundCrossClusterSearchConnection

instance NFData DeleteInboundCrossClusterSearchConnection

instance ToHeaders DeleteInboundCrossClusterSearchConnection where
  toHeaders = const mempty

instance ToPath DeleteInboundCrossClusterSearchConnection where
  toPath DeleteInboundCrossClusterSearchConnection' {..} =
    mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        toBS _diccscCrossClusterSearchConnectionId
      ]

instance ToQuery DeleteInboundCrossClusterSearchConnection where
  toQuery = const mempty

-- | The result of a @'DeleteInboundCrossClusterSearchConnection' @ operation. Contains details of deleted inbound connection.
--
--
--
-- /See:/ 'deleteInboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteInboundCrossClusterSearchConnectionResponse = DeleteInboundCrossClusterSearchConnectionResponse'
  { _diccscirsCrossClusterSearchConnection ::
      !( Maybe
           InboundCrossClusterSearchConnection
       ),
    _diccscirsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diccscirsCrossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection.
--
-- * 'diccscirsResponseStatus' - -- | The response status code.
deleteInboundCrossClusterSearchConnectionResponse ::
  -- | 'diccscirsResponseStatus'
  Int ->
  DeleteInboundCrossClusterSearchConnectionResponse
deleteInboundCrossClusterSearchConnectionResponse pResponseStatus_ =
  DeleteInboundCrossClusterSearchConnectionResponse'
    { _diccscirsCrossClusterSearchConnection =
        Nothing,
      _diccscirsResponseStatus = pResponseStatus_
    }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection.
diccscirsCrossClusterSearchConnection :: Lens' DeleteInboundCrossClusterSearchConnectionResponse (Maybe InboundCrossClusterSearchConnection)
diccscirsCrossClusterSearchConnection = lens _diccscirsCrossClusterSearchConnection (\s a -> s {_diccscirsCrossClusterSearchConnection = a})

-- | -- | The response status code.
diccscirsResponseStatus :: Lens' DeleteInboundCrossClusterSearchConnectionResponse Int
diccscirsResponseStatus = lens _diccscirsResponseStatus (\s a -> s {_diccscirsResponseStatus = a})

instance NFData DeleteInboundCrossClusterSearchConnectionResponse
