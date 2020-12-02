{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicationRule

-- | A container for replication rules. You can add up to 1,000 rules. The maximum size of a replication configuration is 2 MB.
--
--
--
-- /See:/ 'replicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { _rcRole ::
      !Text,
    _rcRules :: ![ReplicationRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'rcRules' - A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules.
replicationConfiguration ::
  -- | 'rcRole'
  Text ->
  ReplicationConfiguration
replicationConfiguration pRole_ =
  ReplicationConfiguration' {_rcRole = pRole_, _rcRules = mempty}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
rcRole :: Lens' ReplicationConfiguration Text
rcRole = lens _rcRole (\s a -> s {_rcRole = a})

-- | A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules.
rcRules :: Lens' ReplicationConfiguration [ReplicationRule]
rcRules = lens _rcRules (\s a -> s {_rcRules = a}) . _Coerce

instance FromXML ReplicationConfiguration where
  parseXML x =
    ReplicationConfiguration'
      <$> (x .@ "Role") <*> (parseXMLList "Rule" x)

instance Hashable ReplicationConfiguration

instance NFData ReplicationConfiguration

instance ToXML ReplicationConfiguration where
  toXML ReplicationConfiguration' {..} =
    mconcat ["Role" @= _rcRole, toXMLList "Rule" _rcRules]
