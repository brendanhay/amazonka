{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CancelResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a resize operation for a cluster.
module Network.AWS.Redshift.CancelResize
  ( -- * Creating a Request
    cancelResize,
    CancelResize,

    -- * Request Lenses
    crClusterIdentifier,

    -- * Destructuring the Response
    resizeProgressMessage,
    ResizeProgressMessage,

    -- * Response Lenses
    rpmImportTablesNotStarted,
    rpmStatus,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmTargetNumberOfNodes,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmImportTablesInProgress,
    rpmResizeType,
    rpmImportTablesCompleted,
    rpmProgressInMegaBytes,
    rpmDataTransferProgressPercent,
    rpmTotalResizeDataInMegaBytes,
    rpmTargetClusterType,
    rpmMessage,
    rpmElapsedTimeInSeconds,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelResize' smart constructor.
newtype CancelResize = CancelResize' {_crClusterIdentifier :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelResize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crClusterIdentifier' - The unique identifier for the cluster that you want to cancel a resize operation for.
cancelResize ::
  -- | 'crClusterIdentifier'
  Text ->
  CancelResize
cancelResize pClusterIdentifier_ =
  CancelResize' {_crClusterIdentifier = pClusterIdentifier_}

-- | The unique identifier for the cluster that you want to cancel a resize operation for.
crClusterIdentifier :: Lens' CancelResize Text
crClusterIdentifier = lens _crClusterIdentifier (\s a -> s {_crClusterIdentifier = a})

instance AWSRequest CancelResize where
  type Rs CancelResize = ResizeProgressMessage
  request = postQuery redshift
  response =
    receiveXMLWrapper "CancelResizeResult" (\s h x -> parseXML x)

instance Hashable CancelResize

instance NFData CancelResize

instance ToHeaders CancelResize where
  toHeaders = const mempty

instance ToPath CancelResize where
  toPath = const "/"

instance ToQuery CancelResize where
  toQuery CancelResize' {..} =
    mconcat
      [ "Action" =: ("CancelResize" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ClusterIdentifier" =: _crClusterIdentifier
      ]
