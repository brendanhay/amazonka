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
-- Module      : Network.AWS.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a manual snapshot of an instance or disk as another manual snapshot, or copies an automatic snapshot of an instance or disk as a manual snapshot. This operation can also be used to copy a manual or automatic snapshot of an instance or a disk from one AWS Region to another in Amazon Lightsail.
--
--
-- When copying a /manual snapshot/ , be sure to define the @source region@ , @source snapshot name@ , and @target snapshot name@ parameters.
--
-- When copying an /automatic snapshot/ , be sure to define the @source region@ , @source resource name@ , @target snapshot name@ , and either the @restore date@ or the @use latest restorable auto snapshot@ parameters.
module Network.AWS.Lightsail.CopySnapshot
  ( -- * Creating a Request
    copySnapshot,
    CopySnapshot,

    -- * Request Lenses
    csUseLatestRestorableAutoSnapshot,
    csRestoreDate,
    csSourceResourceName,
    csSourceSnapshotName,
    csTargetSnapshotName,
    csSourceRegion,

    -- * Destructuring the Response
    copySnapshotResponse,
    CopySnapshotResponse,

    -- * Response Lenses
    csrsOperations,
    csrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { _csUseLatestRestorableAutoSnapshot ::
      !(Maybe Bool),
    _csRestoreDate :: !(Maybe Text),
    _csSourceResourceName :: !(Maybe Text),
    _csSourceSnapshotName :: !(Maybe Text),
    _csTargetSnapshotName :: !Text,
    _csSourceRegion :: !RegionName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csUseLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk. Constraints:     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'csRestoreDate' - The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots. Constraints:     * Must be specified in @YYYY-MM-DD@ format.     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'csSourceResourceName' - The name of the source instance or disk from which the source automatic snapshot was created. Constraint:     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'csSourceSnapshotName' - The name of the source manual snapshot to copy. Constraint:     * Define this parameter only when copying a manual snapshot as another manual snapshot.
--
-- * 'csTargetSnapshotName' - The name of the new manual snapshot to be created as a copy.
--
-- * 'csSourceRegion' - The AWS Region where the source manual or automatic snapshot is located.
copySnapshot ::
  -- | 'csTargetSnapshotName'
  Text ->
  -- | 'csSourceRegion'
  RegionName ->
  CopySnapshot
copySnapshot pTargetSnapshotName_ pSourceRegion_ =
  CopySnapshot'
    { _csUseLatestRestorableAutoSnapshot = Nothing,
      _csRestoreDate = Nothing,
      _csSourceResourceName = Nothing,
      _csSourceSnapshotName = Nothing,
      _csTargetSnapshotName = pTargetSnapshotName_,
      _csSourceRegion = pSourceRegion_
    }

-- | A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk. Constraints:     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
csUseLatestRestorableAutoSnapshot :: Lens' CopySnapshot (Maybe Bool)
csUseLatestRestorableAutoSnapshot = lens _csUseLatestRestorableAutoSnapshot (\s a -> s {_csUseLatestRestorableAutoSnapshot = a})

-- | The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots. Constraints:     * Must be specified in @YYYY-MM-DD@ format.     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
csRestoreDate :: Lens' CopySnapshot (Maybe Text)
csRestoreDate = lens _csRestoreDate (\s a -> s {_csRestoreDate = a})

-- | The name of the source instance or disk from which the source automatic snapshot was created. Constraint:     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
csSourceResourceName :: Lens' CopySnapshot (Maybe Text)
csSourceResourceName = lens _csSourceResourceName (\s a -> s {_csSourceResourceName = a})

-- | The name of the source manual snapshot to copy. Constraint:     * Define this parameter only when copying a manual snapshot as another manual snapshot.
csSourceSnapshotName :: Lens' CopySnapshot (Maybe Text)
csSourceSnapshotName = lens _csSourceSnapshotName (\s a -> s {_csSourceSnapshotName = a})

-- | The name of the new manual snapshot to be created as a copy.
csTargetSnapshotName :: Lens' CopySnapshot Text
csTargetSnapshotName = lens _csTargetSnapshotName (\s a -> s {_csTargetSnapshotName = a})

-- | The AWS Region where the source manual or automatic snapshot is located.
csSourceRegion :: Lens' CopySnapshot RegionName
csSourceRegion = lens _csSourceRegion (\s a -> s {_csSourceRegion = a})

instance AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CopySnapshot

instance NFData CopySnapshot

instance ToHeaders CopySnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CopySnapshot" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CopySnapshot where
  toJSON CopySnapshot' {..} =
    object
      ( catMaybes
          [ ("useLatestRestorableAutoSnapshot" .=)
              <$> _csUseLatestRestorableAutoSnapshot,
            ("restoreDate" .=) <$> _csRestoreDate,
            ("sourceResourceName" .=) <$> _csSourceResourceName,
            ("sourceSnapshotName" .=) <$> _csSourceSnapshotName,
            Just ("targetSnapshotName" .= _csTargetSnapshotName),
            Just ("sourceRegion" .= _csSourceRegion)
          ]
      )

instance ToPath CopySnapshot where
  toPath = const "/"

instance ToQuery CopySnapshot where
  toQuery = const mempty

-- | /See:/ 'copySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { _csrsOperations ::
      !(Maybe [Operation]),
    _csrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'csrsResponseStatus' - -- | The response status code.
copySnapshotResponse ::
  -- | 'csrsResponseStatus'
  Int ->
  CopySnapshotResponse
copySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    { _csrsOperations = Nothing,
      _csrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
csrsOperations :: Lens' CopySnapshotResponse [Operation]
csrsOperations = lens _csrsOperations (\s a -> s {_csrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
csrsResponseStatus :: Lens' CopySnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\s a -> s {_csrsResponseStatus = a})

instance NFData CopySnapshotResponse
