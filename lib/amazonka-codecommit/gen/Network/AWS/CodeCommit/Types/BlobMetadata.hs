{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BlobMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BlobMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific Git blob object.
--
--
--
-- /See:/ 'blobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { _bmPath :: !(Maybe Text),
    _bmMode :: !(Maybe Text),
    _bmBlobId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlobMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmPath' - The path to the blob and associated file name, if any.
--
-- * 'bmMode' - The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
--
-- * 'bmBlobId' - The full ID of the blob.
blobMetadata ::
  BlobMetadata
blobMetadata =
  BlobMetadata'
    { _bmPath = Nothing,
      _bmMode = Nothing,
      _bmBlobId = Nothing
    }

-- | The path to the blob and associated file name, if any.
bmPath :: Lens' BlobMetadata (Maybe Text)
bmPath = lens _bmPath (\s a -> s {_bmPath = a})

-- | The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
bmMode :: Lens' BlobMetadata (Maybe Text)
bmMode = lens _bmMode (\s a -> s {_bmMode = a})

-- | The full ID of the blob.
bmBlobId :: Lens' BlobMetadata (Maybe Text)
bmBlobId = lens _bmBlobId (\s a -> s {_bmBlobId = a})

instance FromJSON BlobMetadata where
  parseJSON =
    withObject
      "BlobMetadata"
      ( \x ->
          BlobMetadata'
            <$> (x .:? "path") <*> (x .:? "mode") <*> (x .:? "blobId")
      )

instance Hashable BlobMetadata

instance NFData BlobMetadata
