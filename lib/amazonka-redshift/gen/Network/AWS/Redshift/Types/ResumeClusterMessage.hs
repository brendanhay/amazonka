{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResumeClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResumeClusterMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a resume cluster operation. For example, a scheduled action to run the @ResumeCluster@ API operation.
--
--
--
-- /See:/ 'resumeClusterMessage' smart constructor.
newtype ResumeClusterMessage = ResumeClusterMessage'
  { _rClusterIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeClusterMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rClusterIdentifier' - The identifier of the cluster to be resumed.
resumeClusterMessage ::
  -- | 'rClusterIdentifier'
  Text ->
  ResumeClusterMessage
resumeClusterMessage pClusterIdentifier_ =
  ResumeClusterMessage' {_rClusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be resumed.
rClusterIdentifier :: Lens' ResumeClusterMessage Text
rClusterIdentifier = lens _rClusterIdentifier (\s a -> s {_rClusterIdentifier = a})

instance FromXML ResumeClusterMessage where
  parseXML x = ResumeClusterMessage' <$> (x .@ "ClusterIdentifier")

instance Hashable ResumeClusterMessage

instance NFData ResumeClusterMessage

instance ToQuery ResumeClusterMessage where
  toQuery ResumeClusterMessage' {..} =
    mconcat ["ClusterIdentifier" =: _rClusterIdentifier]
