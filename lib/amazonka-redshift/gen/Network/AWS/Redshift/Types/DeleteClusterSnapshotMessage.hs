{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- |
--
--
--
-- /See:/ 'deleteClusterSnapshotMessage' smart constructor.
data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage'
  { _dcsmSnapshotClusterIdentifier ::
      !(Maybe Text),
    _dcsmSnapshotIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteClusterSnapshotMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsmSnapshotClusterIdentifier' - The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name. Constraints: Must be the name of valid cluster.
--
-- * 'dcsmSnapshotIdentifier' - The unique identifier of the manual snapshot to be deleted. Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
deleteClusterSnapshotMessage ::
  -- | 'dcsmSnapshotIdentifier'
  Text ->
  DeleteClusterSnapshotMessage
deleteClusterSnapshotMessage pSnapshotIdentifier_ =
  DeleteClusterSnapshotMessage'
    { _dcsmSnapshotClusterIdentifier =
        Nothing,
      _dcsmSnapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name. Constraints: Must be the name of valid cluster.
dcsmSnapshotClusterIdentifier :: Lens' DeleteClusterSnapshotMessage (Maybe Text)
dcsmSnapshotClusterIdentifier = lens _dcsmSnapshotClusterIdentifier (\s a -> s {_dcsmSnapshotClusterIdentifier = a})

-- | The unique identifier of the manual snapshot to be deleted. Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
dcsmSnapshotIdentifier :: Lens' DeleteClusterSnapshotMessage Text
dcsmSnapshotIdentifier = lens _dcsmSnapshotIdentifier (\s a -> s {_dcsmSnapshotIdentifier = a})

instance Hashable DeleteClusterSnapshotMessage

instance NFData DeleteClusterSnapshotMessage

instance ToQuery DeleteClusterSnapshotMessage where
  toQuery DeleteClusterSnapshotMessage' {..} =
    mconcat
      [ "SnapshotClusterIdentifier" =: _dcsmSnapshotClusterIdentifier,
        "SnapshotIdentifier" =: _dcsmSnapshotIdentifier
      ]
