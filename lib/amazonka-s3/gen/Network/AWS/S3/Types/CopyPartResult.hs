{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyPartResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyPartResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
--
--
-- /See:/ 'copyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { _cprETag :: !(Maybe ETag),
    _cprLastModified :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyPartResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprETag' - Entity tag of the object.
--
-- * 'cprLastModified' - Date and time at which the object was uploaded.
copyPartResult ::
  CopyPartResult
copyPartResult =
  CopyPartResult' {_cprETag = Nothing, _cprLastModified = Nothing}

-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\s a -> s {_cprETag = a})

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\s a -> s {_cprLastModified = a}) . mapping _Time

instance FromXML CopyPartResult where
  parseXML x =
    CopyPartResult' <$> (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyPartResult

instance NFData CopyPartResult
