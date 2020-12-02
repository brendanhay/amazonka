{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Category
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Category where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A JSON-formatted name/value pair that represents the category name and category code of the problem, selected from the 'DescribeServices' response for each AWS service.
--
--
--
-- /See:/ 'category' smart constructor.
data Category = Category'
  { _cName :: !(Maybe Text),
    _cCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cName' - The category name for the support case.
--
-- * 'cCode' - The category code for the support case.
category ::
  Category
category = Category' {_cName = Nothing, _cCode = Nothing}

-- | The category name for the support case.
cName :: Lens' Category (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | The category code for the support case.
cCode :: Lens' Category (Maybe Text)
cCode = lens _cCode (\s a -> s {_cCode = a})

instance FromJSON Category where
  parseJSON =
    withObject
      "Category"
      (\x -> Category' <$> (x .:? "name") <*> (x .:? "code"))

instance Hashable Category

instance NFData Category
