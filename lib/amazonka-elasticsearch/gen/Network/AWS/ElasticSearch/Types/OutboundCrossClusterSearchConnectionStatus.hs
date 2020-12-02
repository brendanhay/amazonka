{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus where

import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the connection status of an outbound cross-cluster search connection.
--
--
--
-- /See:/ 'outboundCrossClusterSearchConnectionStatus' smart constructor.
data OutboundCrossClusterSearchConnectionStatus = OutboundCrossClusterSearchConnectionStatus'
  { _occscsMessage ::
      !( Maybe
           Text
       ),
    _occscsStatusCode ::
      !( Maybe
           OutboundCrossClusterSearchConnectionStatusCode
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

-- | Creates a value of 'OutboundCrossClusterSearchConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'occscsMessage' - Specifies verbose information for the outbound connection status.
--
-- * 'occscsStatusCode' - The state code for outbound connection. This can be one of the following:     * VALIDATING: The outbound connection request is being validated.    * VALIDATION_FAILED: Validation failed for the connection request.    * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.    * PROVISIONING: Outbound connection request is in process.    * ACTIVE: Outbound connection is active and ready to use.    * REJECTED: Outbound connection request is rejected by destination domain owner.    * DELETING: Outbound connection deletion is in progress.    * DELETED: Outbound connection is deleted and cannot be used further.
outboundCrossClusterSearchConnectionStatus ::
  OutboundCrossClusterSearchConnectionStatus
outboundCrossClusterSearchConnectionStatus =
  OutboundCrossClusterSearchConnectionStatus'
    { _occscsMessage =
        Nothing,
      _occscsStatusCode = Nothing
    }

-- | Specifies verbose information for the outbound connection status.
occscsMessage :: Lens' OutboundCrossClusterSearchConnectionStatus (Maybe Text)
occscsMessage = lens _occscsMessage (\s a -> s {_occscsMessage = a})

-- | The state code for outbound connection. This can be one of the following:     * VALIDATING: The outbound connection request is being validated.    * VALIDATION_FAILED: Validation failed for the connection request.    * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.    * PROVISIONING: Outbound connection request is in process.    * ACTIVE: Outbound connection is active and ready to use.    * REJECTED: Outbound connection request is rejected by destination domain owner.    * DELETING: Outbound connection deletion is in progress.    * DELETED: Outbound connection is deleted and cannot be used further.
occscsStatusCode :: Lens' OutboundCrossClusterSearchConnectionStatus (Maybe OutboundCrossClusterSearchConnectionStatusCode)
occscsStatusCode = lens _occscsStatusCode (\s a -> s {_occscsStatusCode = a})

instance FromJSON OutboundCrossClusterSearchConnectionStatus where
  parseJSON =
    withObject
      "OutboundCrossClusterSearchConnectionStatus"
      ( \x ->
          OutboundCrossClusterSearchConnectionStatus'
            <$> (x .:? "Message") <*> (x .:? "StatusCode")
      )

instance Hashable OutboundCrossClusterSearchConnectionStatus

instance NFData OutboundCrossClusterSearchConnectionStatus
