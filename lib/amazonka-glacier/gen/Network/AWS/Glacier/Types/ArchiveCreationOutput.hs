{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ArchiveCreationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ArchiveCreationOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the Amazon S3 Glacier response to your request.
--
--
-- For information about the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> . For conceptual information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> .
--
--
-- /See:/ 'archiveCreationOutput' smart constructor.
data ArchiveCreationOutput = ArchiveCreationOutput'
  { _acoArchiveId ::
      !(Maybe Text),
    _acoChecksum :: !(Maybe Text),
    _acoLocation :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArchiveCreationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acoArchiveId' - The ID of the archive. This value is also included as part of the location.
--
-- * 'acoChecksum' - The checksum of the archive computed by Amazon S3 Glacier.
--
-- * 'acoLocation' - The relative URI path of the newly added archive resource.
archiveCreationOutput ::
  ArchiveCreationOutput
archiveCreationOutput =
  ArchiveCreationOutput'
    { _acoArchiveId = Nothing,
      _acoChecksum = Nothing,
      _acoLocation = Nothing
    }

-- | The ID of the archive. This value is also included as part of the location.
acoArchiveId :: Lens' ArchiveCreationOutput (Maybe Text)
acoArchiveId = lens _acoArchiveId (\s a -> s {_acoArchiveId = a})

-- | The checksum of the archive computed by Amazon S3 Glacier.
acoChecksum :: Lens' ArchiveCreationOutput (Maybe Text)
acoChecksum = lens _acoChecksum (\s a -> s {_acoChecksum = a})

-- | The relative URI path of the newly added archive resource.
acoLocation :: Lens' ArchiveCreationOutput (Maybe Text)
acoLocation = lens _acoLocation (\s a -> s {_acoLocation = a})

instance FromJSON ArchiveCreationOutput where
  parseJSON =
    withObject
      "ArchiveCreationOutput"
      ( \x ->
          ArchiveCreationOutput'
            <$> (x .:? "x-amz-archive-id")
            <*> (x .:? "x-amz-sha256-tree-hash")
            <*> (x .:? "Location")
      )

instance Hashable ArchiveCreationOutput

instance NFData ArchiveCreationOutput
