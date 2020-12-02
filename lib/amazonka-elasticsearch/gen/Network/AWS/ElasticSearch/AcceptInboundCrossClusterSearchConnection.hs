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
-- Module      : Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to accept an inbound cross-cluster search connection request.
module Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    acceptInboundCrossClusterSearchConnection,
    AcceptInboundCrossClusterSearchConnection,

    -- * Request Lenses
    aiccscCrossClusterSearchConnectionId,

    -- * Destructuring the Response
    acceptInboundCrossClusterSearchConnectionResponse,
    AcceptInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    aiccscrsCrossClusterSearchConnection,
    aiccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'AcceptInboundCrossClusterSearchConnection' @ operation.
--
--
--
-- /See:/ 'acceptInboundCrossClusterSearchConnection' smart constructor.
newtype AcceptInboundCrossClusterSearchConnection = AcceptInboundCrossClusterSearchConnection'
  { _aiccscCrossClusterSearchConnectionId ::
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

-- | Creates a value of 'AcceptInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiccscCrossClusterSearchConnectionId' - The id of the inbound connection that you want to accept.
acceptInboundCrossClusterSearchConnection ::
  -- | 'aiccscCrossClusterSearchConnectionId'
  Text ->
  AcceptInboundCrossClusterSearchConnection
acceptInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    AcceptInboundCrossClusterSearchConnection'
      { _aiccscCrossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to accept.
aiccscCrossClusterSearchConnectionId :: Lens' AcceptInboundCrossClusterSearchConnection Text
aiccscCrossClusterSearchConnectionId = lens _aiccscCrossClusterSearchConnectionId (\s a -> s {_aiccscCrossClusterSearchConnectionId = a})

instance AWSRequest AcceptInboundCrossClusterSearchConnection where
  type
    Rs AcceptInboundCrossClusterSearchConnection =
      AcceptInboundCrossClusterSearchConnectionResponse
  request = putJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          AcceptInboundCrossClusterSearchConnectionResponse'
            <$> (x .?> "CrossClusterSearchConnection") <*> (pure (fromEnum s))
      )

instance Hashable AcceptInboundCrossClusterSearchConnection

instance NFData AcceptInboundCrossClusterSearchConnection

instance ToHeaders AcceptInboundCrossClusterSearchConnection where
  toHeaders = const mempty

instance ToJSON AcceptInboundCrossClusterSearchConnection where
  toJSON = const (Object mempty)

instance ToPath AcceptInboundCrossClusterSearchConnection where
  toPath AcceptInboundCrossClusterSearchConnection' {..} =
    mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        toBS _aiccscCrossClusterSearchConnectionId,
        "/accept"
      ]

instance ToQuery AcceptInboundCrossClusterSearchConnection where
  toQuery = const mempty

-- | The result of a @'AcceptInboundCrossClusterSearchConnection' @ operation. Contains details of accepted inbound connection.
--
--
--
-- /See:/ 'acceptInboundCrossClusterSearchConnectionResponse' smart constructor.
data AcceptInboundCrossClusterSearchConnectionResponse = AcceptInboundCrossClusterSearchConnectionResponse'
  { _aiccscrsCrossClusterSearchConnection ::
      !( Maybe
           InboundCrossClusterSearchConnection
       ),
    _aiccscrsResponseStatus ::
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

-- | Creates a value of 'AcceptInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiccscrsCrossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of accepted inbound connection.
--
-- * 'aiccscrsResponseStatus' - -- | The response status code.
acceptInboundCrossClusterSearchConnectionResponse ::
  -- | 'aiccscrsResponseStatus'
  Int ->
  AcceptInboundCrossClusterSearchConnectionResponse
acceptInboundCrossClusterSearchConnectionResponse pResponseStatus_ =
  AcceptInboundCrossClusterSearchConnectionResponse'
    { _aiccscrsCrossClusterSearchConnection =
        Nothing,
      _aiccscrsResponseStatus = pResponseStatus_
    }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of accepted inbound connection.
aiccscrsCrossClusterSearchConnection :: Lens' AcceptInboundCrossClusterSearchConnectionResponse (Maybe InboundCrossClusterSearchConnection)
aiccscrsCrossClusterSearchConnection = lens _aiccscrsCrossClusterSearchConnection (\s a -> s {_aiccscrsCrossClusterSearchConnection = a})

-- | -- | The response status code.
aiccscrsResponseStatus :: Lens' AcceptInboundCrossClusterSearchConnectionResponse Int
aiccscrsResponseStatus = lens _aiccscrsResponseStatus (\s a -> s {_aiccscrsResponseStatus = a})

instance NFData AcceptInboundCrossClusterSearchConnectionResponse
