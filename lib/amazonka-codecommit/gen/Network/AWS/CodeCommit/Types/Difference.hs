{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Difference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Difference where

import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a set of differences for a commit specifier.
--
--
--
-- /See:/ 'difference' smart constructor.
data Difference = Difference'
  { _dAfterBlob :: !(Maybe BlobMetadata),
    _dBeforeBlob :: !(Maybe BlobMetadata),
    _dChangeType :: !(Maybe ChangeTypeEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Difference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAfterBlob' - Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dBeforeBlob' - Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dChangeType' - Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
difference ::
  Difference
difference =
  Difference'
    { _dAfterBlob = Nothing,
      _dBeforeBlob = Nothing,
      _dChangeType = Nothing
    }

-- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
dAfterBlob :: Lens' Difference (Maybe BlobMetadata)
dAfterBlob = lens _dAfterBlob (\s a -> s {_dAfterBlob = a})

-- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
dBeforeBlob :: Lens' Difference (Maybe BlobMetadata)
dBeforeBlob = lens _dBeforeBlob (\s a -> s {_dBeforeBlob = a})

-- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
dChangeType :: Lens' Difference (Maybe ChangeTypeEnum)
dChangeType = lens _dChangeType (\s a -> s {_dChangeType = a})

instance FromJSON Difference where
  parseJSON =
    withObject
      "Difference"
      ( \x ->
          Difference'
            <$> (x .:? "afterBlob")
            <*> (x .:? "beforeBlob")
            <*> (x .:? "changeType")
      )

instance Hashable Difference

instance NFData Difference
