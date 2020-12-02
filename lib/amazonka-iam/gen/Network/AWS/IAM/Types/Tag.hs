{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that represents user-provided metadata that can be associated with a resource such as an IAM user or role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagKey :: !Text, _tagValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
--
-- * 'tagValue' - The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
tag ::
  -- | 'tagKey'
  Text ->
  -- | 'tagValue'
  Text ->
  Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}

-- | The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

-- | The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

instance FromXML Tag where
  parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag

instance NFData Tag

instance ToQuery Tag where
  toQuery Tag' {..} = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]
