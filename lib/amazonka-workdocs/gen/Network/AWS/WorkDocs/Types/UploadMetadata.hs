{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UploadMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UploadMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the upload.
--
--
--
-- /See:/ 'uploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { _umUploadURL ::
      !(Maybe (Sensitive Text)),
    _umSignedHeaders :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UploadMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umUploadURL' - The URL of the upload.
--
-- * 'umSignedHeaders' - The signed headers.
uploadMetadata ::
  UploadMetadata
uploadMetadata =
  UploadMetadata'
    { _umUploadURL = Nothing,
      _umSignedHeaders = Nothing
    }

-- | The URL of the upload.
umUploadURL :: Lens' UploadMetadata (Maybe Text)
umUploadURL = lens _umUploadURL (\s a -> s {_umUploadURL = a}) . mapping _Sensitive

-- | The signed headers.
umSignedHeaders :: Lens' UploadMetadata (HashMap Text (Text))
umSignedHeaders = lens _umSignedHeaders (\s a -> s {_umSignedHeaders = a}) . _Default . _Map

instance FromJSON UploadMetadata where
  parseJSON =
    withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            <$> (x .:? "UploadUrl") <*> (x .:? "SignedHeaders" .!= mempty)
      )

instance Hashable UploadMetadata

instance NFData UploadMetadata
