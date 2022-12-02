{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Amazonka.EC2.Internal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Internal where

import Amazonka.Core.Lens.Internal
import Amazonka.Data
import Amazonka.Prelude

-- | Custom 'Tag' type which has an optional value component.
--
-- /See:/ 'tag' smart constructor.
data DeleteTag = DeleteTag'
  { key :: !Text,
    value :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Generic)

newDeleteTag ::
  -- | 'key'
  Text ->
  DeleteTag
newDeleteTag k = DeleteTag' k Nothing

-- | The key of the tag to delete.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127
-- Unicode characters. May not begin with 'aws:'
deleteTag_key :: Lens' DeleteTag Text
deleteTag_key = lens key (\s a -> s {key = a})

-- | The optional value of the tag to delete.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
deleteTag_value :: Lens' DeleteTag (Maybe Text)
deleteTag_value = lens value (\s a -> s {value = a})

instance FromXML DeleteTag where
  parseXML x = DeleteTag' <$> (x .@ "key") <*> (x .@? "value")

instance ToQuery DeleteTag where
  toQuery DeleteTag' {..} =
    mconcat
      [ "Key" =: key,
        "Value" =: value
      ]
