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

import Amazonka.Core
import Amazonka.Core.Lens.Internal
import Amazonka.Prelude

-- | Custom 'Tag' type which has an optional value component.
--
-- /See:/ 'tag' smart constructor.
data DeleteTag = DeleteTag
  { _deleteTagKey :: !Text,
    _deleteTagValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Generic)

deleteTag ::
  -- | 'deleteTagKey'
  Text ->
  DeleteTag
deleteTag k = DeleteTag k Nothing

-- | The key of the tag to delete.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127
-- Unicode characters. May not begin with 'aws:'
deleteTagKey :: Lens' DeleteTag Text
deleteTagKey = lens _deleteTagKey (\s a -> s {_deleteTagKey = a})

-- | The optional value of the tag to delete.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
deleteTagValue :: Lens' DeleteTag (Maybe Text)
deleteTagValue = lens _deleteTagValue (\s a -> s {_deleteTagValue = a})

instance FromXML DeleteTag where
  parseXML x = DeleteTag <$> (x .@ "key") <*> (x .@? "value")

instance ToQuery DeleteTag where
  toQuery DeleteTag {..} =
    mconcat
      [ "Key" =: _deleteTagKey,
        "Value" =: _deleteTagValue
      ]
