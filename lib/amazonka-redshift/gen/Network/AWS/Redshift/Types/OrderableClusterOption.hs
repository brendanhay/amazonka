{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OrderableClusterOption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AvailabilityZone

-- | Describes an orderable cluster option.
--
--
--
-- /See:/ 'orderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { _ocoAvailabilityZones ::
      !(Maybe [AvailabilityZone]),
    _ocoClusterType :: !(Maybe Text),
    _ocoClusterVersion :: !(Maybe Text),
    _ocoNodeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrderableClusterOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocoAvailabilityZones' - A list of availability zones for the orderable cluster.
--
-- * 'ocoClusterType' - The cluster type, for example @multi-node@ .
--
-- * 'ocoClusterVersion' - The version of the orderable cluster.
--
-- * 'ocoNodeType' - The node type for the orderable cluster.
orderableClusterOption ::
  OrderableClusterOption
orderableClusterOption =
  OrderableClusterOption'
    { _ocoAvailabilityZones = Nothing,
      _ocoClusterType = Nothing,
      _ocoClusterVersion = Nothing,
      _ocoNodeType = Nothing
    }

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption [AvailabilityZone]
ocoAvailabilityZones = lens _ocoAvailabilityZones (\s a -> s {_ocoAvailabilityZones = a}) . _Default . _Coerce

-- | The cluster type, for example @multi-node@ .
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\s a -> s {_ocoClusterType = a})

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion = lens _ocoClusterVersion (\s a -> s {_ocoClusterVersion = a})

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\s a -> s {_ocoNodeType = a})

instance FromXML OrderableClusterOption where
  parseXML x =
    OrderableClusterOption'
      <$> ( x .@? "AvailabilityZones" .!@ mempty
              >>= may (parseXMLList "AvailabilityZone")
          )
      <*> (x .@? "ClusterType")
      <*> (x .@? "ClusterVersion")
      <*> (x .@? "NodeType")

instance Hashable OrderableClusterOption

instance NFData OrderableClusterOption
