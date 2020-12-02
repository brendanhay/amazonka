{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes a parameter group.
--
--
--
-- /See:/ 'clusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { _cpgParameterGroupFamily ::
      !(Maybe Text),
    _cpgDescription :: !(Maybe Text),
    _cpgTags :: !(Maybe [Tag]),
    _cpgParameterGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgParameterGroupFamily' - The name of the cluster parameter group family that this cluster parameter group is compatible with.
--
-- * 'cpgDescription' - The description of the parameter group.
--
-- * 'cpgTags' - The list of tags for the cluster parameter group.
--
-- * 'cpgParameterGroupName' - The name of the cluster parameter group.
clusterParameterGroup ::
  ClusterParameterGroup
clusterParameterGroup =
  ClusterParameterGroup'
    { _cpgParameterGroupFamily = Nothing,
      _cpgDescription = Nothing,
      _cpgTags = Nothing,
      _cpgParameterGroupName = Nothing
    }

-- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily = lens _cpgParameterGroupFamily (\s a -> s {_cpgParameterGroupFamily = a})

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s {_cpgDescription = a})

-- | The list of tags for the cluster parameter group.
cpgTags :: Lens' ClusterParameterGroup [Tag]
cpgTags = lens _cpgTags (\s a -> s {_cpgTags = a}) . _Default . _Coerce

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName = lens _cpgParameterGroupName (\s a -> s {_cpgParameterGroupName = a})

instance FromXML ClusterParameterGroup where
  parseXML x =
    ClusterParameterGroup'
      <$> (x .@? "ParameterGroupFamily")
      <*> (x .@? "Description")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "ParameterGroupName")

instance Hashable ClusterParameterGroup

instance NFData ClusterParameterGroup
