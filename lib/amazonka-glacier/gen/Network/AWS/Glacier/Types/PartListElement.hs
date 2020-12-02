{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.PartListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.PartListElement where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of the part sizes of the multipart upload.
--
--
--
-- /See:/ 'partListElement' smart constructor.
data PartListElement = PartListElement'
  { _pleSHA256TreeHash ::
      !(Maybe Text),
    _pleRangeInBytes :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartListElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pleSHA256TreeHash' - The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
--
-- * 'pleRangeInBytes' - The byte range of a part, inclusive of the upper value of the range.
partListElement ::
  PartListElement
partListElement =
  PartListElement'
    { _pleSHA256TreeHash = Nothing,
      _pleRangeInBytes = Nothing
    }

-- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
pleSHA256TreeHash :: Lens' PartListElement (Maybe Text)
pleSHA256TreeHash = lens _pleSHA256TreeHash (\s a -> s {_pleSHA256TreeHash = a})

-- | The byte range of a part, inclusive of the upper value of the range.
pleRangeInBytes :: Lens' PartListElement (Maybe Text)
pleRangeInBytes = lens _pleRangeInBytes (\s a -> s {_pleRangeInBytes = a})

instance FromJSON PartListElement where
  parseJSON =
    withObject
      "PartListElement"
      ( \x ->
          PartListElement'
            <$> (x .:? "SHA256TreeHash") <*> (x .:? "RangeInBytes")
      )

instance Hashable PartListElement

instance NFData PartListElement
