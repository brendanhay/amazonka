{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ErrorDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ErrorDocument where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The error information.
--
--
--
-- /See:/ 'errorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument' {_edKey :: ObjectKey}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edKey' - The object key name to use when a 4XX class error occurs.
errorDocument ::
  -- | 'edKey'
  ObjectKey ->
  ErrorDocument
errorDocument pKey_ = ErrorDocument' {_edKey = pKey_}

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\s a -> s {_edKey = a})

instance FromXML ErrorDocument where
  parseXML x = ErrorDocument' <$> (x .@ "Key")

instance Hashable ErrorDocument

instance NFData ErrorDocument

instance ToXML ErrorDocument where
  toXML ErrorDocument' {..} = mconcat ["Key" @= _edKey]
