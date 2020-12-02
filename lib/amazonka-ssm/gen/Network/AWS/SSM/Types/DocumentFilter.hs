{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DocumentFilterKey

-- | This data type is deprecated. Instead, use 'DocumentKeyValuesFilter' .
--
--
--
-- /See:/ 'documentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { _dfKey :: !DocumentFilterKey,
    _dfValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfKey' - The name of the filter.
--
-- * 'dfValue' - The value of the filter.
documentFilter ::
  -- | 'dfKey'
  DocumentFilterKey ->
  -- | 'dfValue'
  Text ->
  DocumentFilter
documentFilter pKey_ pValue_ =
  DocumentFilter' {_dfKey = pKey_, _dfValue = pValue_}

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\s a -> s {_dfKey = a})

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\s a -> s {_dfValue = a})

instance Hashable DocumentFilter

instance NFData DocumentFilter

instance ToJSON DocumentFilter where
  toJSON DocumentFilter' {..} =
    object
      (catMaybes [Just ("key" .= _dfKey), Just ("value" .= _dfValue)])
