{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotCopyGrant where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.
--
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- /See:/ 'snapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { _scgKMSKeyId ::
      !(Maybe Text),
    _scgSnapshotCopyGrantName :: !(Maybe Text),
    _scgTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotCopyGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scgKMSKeyId' - The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
--
-- * 'scgSnapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- * 'scgTags' - A list of tag instances.
snapshotCopyGrant ::
  SnapshotCopyGrant
snapshotCopyGrant =
  SnapshotCopyGrant'
    { _scgKMSKeyId = Nothing,
      _scgSnapshotCopyGrantName = Nothing,
      _scgTags = Nothing
    }

-- | The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
scgKMSKeyId :: Lens' SnapshotCopyGrant (Maybe Text)
scgKMSKeyId = lens _scgKMSKeyId (\s a -> s {_scgKMSKeyId = a})

-- | The name of the snapshot copy grant.
scgSnapshotCopyGrantName :: Lens' SnapshotCopyGrant (Maybe Text)
scgSnapshotCopyGrantName = lens _scgSnapshotCopyGrantName (\s a -> s {_scgSnapshotCopyGrantName = a})

-- | A list of tag instances.
scgTags :: Lens' SnapshotCopyGrant [Tag]
scgTags = lens _scgTags (\s a -> s {_scgTags = a}) . _Default . _Coerce

instance FromXML SnapshotCopyGrant where
  parseXML x =
    SnapshotCopyGrant'
      <$> (x .@? "KmsKeyId")
      <*> (x .@? "SnapshotCopyGrantName")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable SnapshotCopyGrant

instance NFData SnapshotCopyGrant
