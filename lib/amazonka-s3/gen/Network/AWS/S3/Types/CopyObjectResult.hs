{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyObjectResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyObjectResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
--
--
-- /See:/ 'copyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { _corETag ::
      !(Maybe ETag),
    _corLastModified :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyObjectResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corETag' - Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
--
-- * 'corLastModified' - Returns the date that the object was last modified.
copyObjectResult ::
  CopyObjectResult
copyObjectResult =
  CopyObjectResult' {_corETag = Nothing, _corLastModified = Nothing}

-- | Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
corETag :: Lens' CopyObjectResult (Maybe ETag)
corETag = lens _corETag (\s a -> s {_corETag = a})

-- | Returns the date that the object was last modified.
corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\s a -> s {_corLastModified = a}) . mapping _Time

instance FromXML CopyObjectResult where
  parseXML x =
    CopyObjectResult' <$> (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyObjectResult

instance NFData CopyObjectResult
