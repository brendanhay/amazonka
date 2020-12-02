{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Connection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the connection between an endpoint and a replication instance, including Amazon Resource Names (ARNs) and the last error message issued.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _cStatus :: !(Maybe Text),
    _cReplicationInstanceARN :: !(Maybe Text),
    _cEndpointIdentifier :: !(Maybe Text),
    _cReplicationInstanceIdentifier :: !(Maybe Text),
    _cEndpointARN :: !(Maybe Text),
    _cLastFailureMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The connection status. This parameter can return one of the following values:     * @"successful"@      * @"testing"@      * @"failed"@      * @"deleting"@
--
-- * 'cReplicationInstanceARN' - The ARN of the replication instance.
--
-- * 'cEndpointIdentifier' - The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- * 'cReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
--
-- * 'cEndpointARN' - The ARN string that uniquely identifies the endpoint.
--
-- * 'cLastFailureMessage' - The error message when the connection last failed.
connection ::
  Connection
connection =
  Connection'
    { _cStatus = Nothing,
      _cReplicationInstanceARN = Nothing,
      _cEndpointIdentifier = Nothing,
      _cReplicationInstanceIdentifier = Nothing,
      _cEndpointARN = Nothing,
      _cLastFailureMessage = Nothing
    }

-- | The connection status. This parameter can return one of the following values:     * @"successful"@      * @"testing"@      * @"failed"@      * @"deleting"@
cStatus :: Lens' Connection (Maybe Text)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The ARN of the replication instance.
cReplicationInstanceARN :: Lens' Connection (Maybe Text)
cReplicationInstanceARN = lens _cReplicationInstanceARN (\s a -> s {_cReplicationInstanceARN = a})

-- | The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
cEndpointIdentifier :: Lens' Connection (Maybe Text)
cEndpointIdentifier = lens _cEndpointIdentifier (\s a -> s {_cEndpointIdentifier = a})

-- | The replication instance identifier. This parameter is stored as a lowercase string.
cReplicationInstanceIdentifier :: Lens' Connection (Maybe Text)
cReplicationInstanceIdentifier = lens _cReplicationInstanceIdentifier (\s a -> s {_cReplicationInstanceIdentifier = a})

-- | The ARN string that uniquely identifies the endpoint.
cEndpointARN :: Lens' Connection (Maybe Text)
cEndpointARN = lens _cEndpointARN (\s a -> s {_cEndpointARN = a})

-- | The error message when the connection last failed.
cLastFailureMessage :: Lens' Connection (Maybe Text)
cLastFailureMessage = lens _cLastFailureMessage (\s a -> s {_cLastFailureMessage = a})

instance FromJSON Connection where
  parseJSON =
    withObject
      "Connection"
      ( \x ->
          Connection'
            <$> (x .:? "Status")
            <*> (x .:? "ReplicationInstanceArn")
            <*> (x .:? "EndpointIdentifier")
            <*> (x .:? "ReplicationInstanceIdentifier")
            <*> (x .:? "EndpointArn")
            <*> (x .:? "LastFailureMessage")
      )

instance Hashable Connection

instance NFData Connection
