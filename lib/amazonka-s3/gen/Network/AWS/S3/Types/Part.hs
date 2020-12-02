{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Part
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Part where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for elements related to a part.
--
--
--
-- /See:/ 'part' smart constructor.
data Part = Part'
  { _pETag :: !(Maybe ETag),
    _pSize :: !(Maybe Int),
    _pPartNumber :: !(Maybe Int),
    _pLastModified :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Part' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pETag' - Entity tag returned when the part was uploaded.
--
-- * 'pSize' - Size in bytes of the uploaded part data.
--
-- * 'pPartNumber' - Part number identifying the part. This is a positive integer between 1 and 10,000.
--
-- * 'pLastModified' - Date and time at which the part was uploaded.
part ::
  Part
part =
  Part'
    { _pETag = Nothing,
      _pSize = Nothing,
      _pPartNumber = Nothing,
      _pLastModified = Nothing
    }

-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe ETag)
pETag = lens _pETag (\s a -> s {_pETag = a})

-- | Size in bytes of the uploaded part data.
pSize :: Lens' Part (Maybe Int)
pSize = lens _pSize (\s a -> s {_pSize = a})

-- | Part number identifying the part. This is a positive integer between 1 and 10,000.
pPartNumber :: Lens' Part (Maybe Int)
pPartNumber = lens _pPartNumber (\s a -> s {_pPartNumber = a})

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe UTCTime)
pLastModified = lens _pLastModified (\s a -> s {_pLastModified = a}) . mapping _Time

instance FromXML Part where
  parseXML x =
    Part'
      <$> (x .@? "ETag")
      <*> (x .@? "Size")
      <*> (x .@? "PartNumber")
      <*> (x .@? "LastModified")

instance Hashable Part

instance NFData Part
