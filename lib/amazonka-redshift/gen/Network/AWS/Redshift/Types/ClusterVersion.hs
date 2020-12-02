{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterVersion where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a cluster version, including the parameter group family and description of the version.
--
--
--
-- /See:/ 'clusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { _cvClusterParameterGroupFamily ::
      !(Maybe Text),
    _cvClusterVersion :: !(Maybe Text),
    _cvDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvClusterParameterGroupFamily' - The name of the cluster parameter group family for the cluster.
--
-- * 'cvClusterVersion' - The version number used by the cluster.
--
-- * 'cvDescription' - The description of the cluster version.
clusterVersion ::
  ClusterVersion
clusterVersion =
  ClusterVersion'
    { _cvClusterParameterGroupFamily = Nothing,
      _cvClusterVersion = Nothing,
      _cvDescription = Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily = lens _cvClusterParameterGroupFamily (\s a -> s {_cvClusterParameterGroupFamily = a})

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion = lens _cvClusterVersion (\s a -> s {_cvClusterVersion = a})

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\s a -> s {_cvDescription = a})

instance FromXML ClusterVersion where
  parseXML x =
    ClusterVersion'
      <$> (x .@? "ClusterParameterGroupFamily")
      <*> (x .@? "ClusterVersion")
      <*> (x .@? "Description")

instance Hashable ClusterVersion

instance NFData ClusterVersion
