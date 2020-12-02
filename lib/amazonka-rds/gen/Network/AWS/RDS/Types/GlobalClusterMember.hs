{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalClusterMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalClusterMember where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.WriteForwardingStatus

-- | A data structure with information about any primary and secondary clusters associated with an Aurora global database.
--
--
--
-- /See:/ 'globalClusterMember' smart constructor.
data GlobalClusterMember = GlobalClusterMember'
  { _gcmReaders ::
      !(Maybe [Text]),
    _gcmDBClusterARN :: !(Maybe Text),
    _gcmIsWriter :: !(Maybe Bool),
    _gcmGlobalWriteForwardingStatus ::
      !(Maybe WriteForwardingStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalClusterMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmReaders' - The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database.
--
-- * 'gcmDBClusterARN' - The Amazon Resource Name (ARN) for each Aurora cluster.
--
-- * 'gcmIsWriter' - Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated.
--
-- * 'gcmGlobalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
globalClusterMember ::
  GlobalClusterMember
globalClusterMember =
  GlobalClusterMember'
    { _gcmReaders = Nothing,
      _gcmDBClusterARN = Nothing,
      _gcmIsWriter = Nothing,
      _gcmGlobalWriteForwardingStatus = Nothing
    }

-- | The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database.
gcmReaders :: Lens' GlobalClusterMember [Text]
gcmReaders = lens _gcmReaders (\s a -> s {_gcmReaders = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) for each Aurora cluster.
gcmDBClusterARN :: Lens' GlobalClusterMember (Maybe Text)
gcmDBClusterARN = lens _gcmDBClusterARN (\s a -> s {_gcmDBClusterARN = a})

-- | Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated.
gcmIsWriter :: Lens' GlobalClusterMember (Maybe Bool)
gcmIsWriter = lens _gcmIsWriter (\s a -> s {_gcmIsWriter = a})

-- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
gcmGlobalWriteForwardingStatus :: Lens' GlobalClusterMember (Maybe WriteForwardingStatus)
gcmGlobalWriteForwardingStatus = lens _gcmGlobalWriteForwardingStatus (\s a -> s {_gcmGlobalWriteForwardingStatus = a})

instance FromXML GlobalClusterMember where
  parseXML x =
    GlobalClusterMember'
      <$> (x .@? "Readers" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "DBClusterArn")
      <*> (x .@? "IsWriter")
      <*> (x .@? "GlobalWriteForwardingStatus")

instance Hashable GlobalClusterMember

instance NFData GlobalClusterMember
