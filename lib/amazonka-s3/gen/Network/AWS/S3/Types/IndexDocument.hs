{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IndexDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IndexDocument where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for the @Suffix@ element.
--
--
--
-- /See:/ 'indexDocument' smart constructor.
newtype IndexDocument = IndexDocument' {_idSuffix :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IndexDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSuffix' - A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
indexDocument ::
  -- | 'idSuffix'
  Text ->
  IndexDocument
indexDocument pSuffix_ = IndexDocument' {_idSuffix = pSuffix_}

-- | A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\s a -> s {_idSuffix = a})

instance FromXML IndexDocument where
  parseXML x = IndexDocument' <$> (x .@ "Suffix")

instance Hashable IndexDocument

instance NFData IndexDocument

instance ToXML IndexDocument where
  toXML IndexDocument' {..} = mconcat ["Suffix" @= _idSuffix]
