{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamFile where

import Network.AWS.IoT.Types.S3Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a file to stream.
--
--
--
-- /See:/ 'streamFile' smart constructor.
data StreamFile = StreamFile'
  { _sfS3Location :: !(Maybe S3Location),
    _sfFileId :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfS3Location' - The location of the file in S3.
--
-- * 'sfFileId' - The file ID.
streamFile ::
  StreamFile
streamFile =
  StreamFile' {_sfS3Location = Nothing, _sfFileId = Nothing}

-- | The location of the file in S3.
sfS3Location :: Lens' StreamFile (Maybe S3Location)
sfS3Location = lens _sfS3Location (\s a -> s {_sfS3Location = a})

-- | The file ID.
sfFileId :: Lens' StreamFile (Maybe Natural)
sfFileId = lens _sfFileId (\s a -> s {_sfFileId = a}) . mapping _Nat

instance FromJSON StreamFile where
  parseJSON =
    withObject
      "StreamFile"
      (\x -> StreamFile' <$> (x .:? "s3Location") <*> (x .:? "fileId"))

instance Hashable StreamFile

instance NFData StreamFile

instance ToJSON StreamFile where
  toJSON StreamFile' {..} =
    object
      ( catMaybes
          [("s3Location" .=) <$> _sfS3Location, ("fileId" .=) <$> _sfFileId]
      )
