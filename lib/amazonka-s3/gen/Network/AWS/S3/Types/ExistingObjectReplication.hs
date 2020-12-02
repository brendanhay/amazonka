{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExistingObjectReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExistingObjectReplication where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExistingObjectReplicationStatus

-- | Optional configuration to replicate existing source bucket objects. For more information, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects> in the /Amazon S3 Developer Guide/ .
--
--
--
-- /See:/ 'existingObjectReplication' smart constructor.
newtype ExistingObjectReplication = ExistingObjectReplication'
  { _eorStatus ::
      ExistingObjectReplicationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExistingObjectReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eorStatus' -
existingObjectReplication ::
  -- | 'eorStatus'
  ExistingObjectReplicationStatus ->
  ExistingObjectReplication
existingObjectReplication pStatus_ =
  ExistingObjectReplication' {_eorStatus = pStatus_}

-- |
eorStatus :: Lens' ExistingObjectReplication ExistingObjectReplicationStatus
eorStatus = lens _eorStatus (\s a -> s {_eorStatus = a})

instance FromXML ExistingObjectReplication where
  parseXML x = ExistingObjectReplication' <$> (x .@ "Status")

instance Hashable ExistingObjectReplication

instance NFData ExistingObjectReplication

instance ToXML ExistingObjectReplication where
  toXML ExistingObjectReplication' {..} =
    mconcat ["Status" @= _eorStatus]
