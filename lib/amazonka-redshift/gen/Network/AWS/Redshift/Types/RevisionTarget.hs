{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RevisionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RevisionTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a @RevisionTarget@ .
--
--
--
-- /See:/ 'revisionTarget' smart constructor.
data RevisionTarget = RevisionTarget'
  { _rtDatabaseRevisionReleaseDate ::
      !(Maybe ISO8601),
    _rtDatabaseRevision :: !(Maybe Text),
    _rtDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevisionTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtDatabaseRevisionReleaseDate' - The date on which the database revision was released.
--
-- * 'rtDatabaseRevision' - A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
--
-- * 'rtDescription' - A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
revisionTarget ::
  RevisionTarget
revisionTarget =
  RevisionTarget'
    { _rtDatabaseRevisionReleaseDate = Nothing,
      _rtDatabaseRevision = Nothing,
      _rtDescription = Nothing
    }

-- | The date on which the database revision was released.
rtDatabaseRevisionReleaseDate :: Lens' RevisionTarget (Maybe UTCTime)
rtDatabaseRevisionReleaseDate = lens _rtDatabaseRevisionReleaseDate (\s a -> s {_rtDatabaseRevisionReleaseDate = a}) . mapping _Time

-- | A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
rtDatabaseRevision :: Lens' RevisionTarget (Maybe Text)
rtDatabaseRevision = lens _rtDatabaseRevision (\s a -> s {_rtDatabaseRevision = a})

-- | A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
rtDescription :: Lens' RevisionTarget (Maybe Text)
rtDescription = lens _rtDescription (\s a -> s {_rtDescription = a})

instance FromXML RevisionTarget where
  parseXML x =
    RevisionTarget'
      <$> (x .@? "DatabaseRevisionReleaseDate")
      <*> (x .@? "DatabaseRevision")
      <*> (x .@? "Description")

instance Hashable RevisionTarget

instance NFData RevisionTarget
