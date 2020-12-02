{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus where

import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the coonection status of an inbound cross-cluster search connection.
--
--
--
-- /See:/ 'inboundCrossClusterSearchConnectionStatus' smart constructor.
data InboundCrossClusterSearchConnectionStatus = InboundCrossClusterSearchConnectionStatus'
  { _iccscsMessage ::
      !( Maybe
           Text
       ),
    _iccscsStatusCode ::
      !( Maybe
           InboundCrossClusterSearchConnectionStatusCode
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'InboundCrossClusterSearchConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iccscsMessage' - Specifies verbose information for the inbound connection status.
--
-- * 'iccscsStatusCode' - The state code for inbound connection. This can be one of the following:     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.    * APPROVED: Inbound connection is pending acceptance by destination domain owner.    * REJECTING: Inbound connection rejection is in process.    * REJECTED: Inbound connection is rejected.    * DELETING: Inbound connection deletion is in progress.    * DELETED: Inbound connection is deleted and cannot be used further.
inboundCrossClusterSearchConnectionStatus ::
  InboundCrossClusterSearchConnectionStatus
inboundCrossClusterSearchConnectionStatus =
  InboundCrossClusterSearchConnectionStatus'
    { _iccscsMessage =
        Nothing,
      _iccscsStatusCode = Nothing
    }

-- | Specifies verbose information for the inbound connection status.
iccscsMessage :: Lens' InboundCrossClusterSearchConnectionStatus (Maybe Text)
iccscsMessage = lens _iccscsMessage (\s a -> s {_iccscsMessage = a})

-- | The state code for inbound connection. This can be one of the following:     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.    * APPROVED: Inbound connection is pending acceptance by destination domain owner.    * REJECTING: Inbound connection rejection is in process.    * REJECTED: Inbound connection is rejected.    * DELETING: Inbound connection deletion is in progress.    * DELETED: Inbound connection is deleted and cannot be used further.
iccscsStatusCode :: Lens' InboundCrossClusterSearchConnectionStatus (Maybe InboundCrossClusterSearchConnectionStatusCode)
iccscsStatusCode = lens _iccscsStatusCode (\s a -> s {_iccscsStatusCode = a})

instance FromJSON InboundCrossClusterSearchConnectionStatus where
  parseJSON =
    withObject
      "InboundCrossClusterSearchConnectionStatus"
      ( \x ->
          InboundCrossClusterSearchConnectionStatus'
            <$> (x .:? "Message") <*> (x .:? "StatusCode")
      )

instance Hashable InboundCrossClusterSearchConnectionStatus

instance NFData InboundCrossClusterSearchConnectionStatus
