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
-- Module      : Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to reject an inbound cross-cluster search connection request.
module Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    rejectInboundCrossClusterSearchConnection,
    RejectInboundCrossClusterSearchConnection,

    -- * Request Lenses
    riccscCrossClusterSearchConnectionId,

    -- * Destructuring the Response
    rejectInboundCrossClusterSearchConnectionResponse,
    RejectInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    riccscrsCrossClusterSearchConnection,
    riccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'RejectInboundCrossClusterSearchConnection' @ operation.
--
--
--
-- /See:/ 'rejectInboundCrossClusterSearchConnection' smart constructor.
newtype RejectInboundCrossClusterSearchConnection = RejectInboundCrossClusterSearchConnection'
  { _riccscCrossClusterSearchConnectionId ::
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

-- | Creates a value of 'RejectInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riccscCrossClusterSearchConnectionId' - The id of the inbound connection that you want to reject.
rejectInboundCrossClusterSearchConnection ::
  -- | 'riccscCrossClusterSearchConnectionId'
  Text ->
  RejectInboundCrossClusterSearchConnection
rejectInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    RejectInboundCrossClusterSearchConnection'
      { _riccscCrossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to reject.
riccscCrossClusterSearchConnectionId :: Lens' RejectInboundCrossClusterSearchConnection Text
riccscCrossClusterSearchConnectionId = lens _riccscCrossClusterSearchConnectionId (\s a -> s {_riccscCrossClusterSearchConnectionId = a})

instance AWSRequest RejectInboundCrossClusterSearchConnection where
  type
    Rs RejectInboundCrossClusterSearchConnection =
      RejectInboundCrossClusterSearchConnectionResponse
  request = putJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          RejectInboundCrossClusterSearchConnectionResponse'
            <$> (x .?> "CrossClusterSearchConnection") <*> (pure (fromEnum s))
      )

instance Hashable RejectInboundCrossClusterSearchConnection

instance NFData RejectInboundCrossClusterSearchConnection

instance ToHeaders RejectInboundCrossClusterSearchConnection where
  toHeaders = const mempty

instance ToJSON RejectInboundCrossClusterSearchConnection where
  toJSON = const (Object mempty)

instance ToPath RejectInboundCrossClusterSearchConnection where
  toPath RejectInboundCrossClusterSearchConnection' {..} =
    mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        toBS _riccscCrossClusterSearchConnectionId,
        "/reject"
      ]

instance ToQuery RejectInboundCrossClusterSearchConnection where
  toQuery = const mempty

-- | The result of a @'RejectInboundCrossClusterSearchConnection' @ operation. Contains details of rejected inbound connection.
--
--
--
-- /See:/ 'rejectInboundCrossClusterSearchConnectionResponse' smart constructor.
data RejectInboundCrossClusterSearchConnectionResponse = RejectInboundCrossClusterSearchConnectionResponse'
  { _riccscrsCrossClusterSearchConnection ::
      !( Maybe
           InboundCrossClusterSearchConnection
       ),
    _riccscrsResponseStatus ::
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

-- | Creates a value of 'RejectInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riccscrsCrossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection.
--
-- * 'riccscrsResponseStatus' - -- | The response status code.
rejectInboundCrossClusterSearchConnectionResponse ::
  -- | 'riccscrsResponseStatus'
  Int ->
  RejectInboundCrossClusterSearchConnectionResponse
rejectInboundCrossClusterSearchConnectionResponse pResponseStatus_ =
  RejectInboundCrossClusterSearchConnectionResponse'
    { _riccscrsCrossClusterSearchConnection =
        Nothing,
      _riccscrsResponseStatus = pResponseStatus_
    }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection.
riccscrsCrossClusterSearchConnection :: Lens' RejectInboundCrossClusterSearchConnectionResponse (Maybe InboundCrossClusterSearchConnection)
riccscrsCrossClusterSearchConnection = lens _riccscrsCrossClusterSearchConnection (\s a -> s {_riccscrsCrossClusterSearchConnection = a})

-- | -- | The response status code.
riccscrsResponseStatus :: Lens' RejectInboundCrossClusterSearchConnectionResponse Int
riccscrsResponseStatus = lens _riccscrsResponseStatus (\s a -> s {_riccscrsResponseStatus = a})

instance NFData RejectInboundCrossClusterSearchConnectionResponse
