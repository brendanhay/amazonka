{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameQuery where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Part of the @SuggestionQuery@ type. Specifies a hint for retrieving property names that begin with the specified text.
--
--
--
-- /See:/ 'propertyNameQuery' smart constructor.
newtype PropertyNameQuery = PropertyNameQuery'
  { _pnqPropertyNameHint ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PropertyNameQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnqPropertyNameHint' - Text that begins a property's name.
propertyNameQuery ::
  -- | 'pnqPropertyNameHint'
  Text ->
  PropertyNameQuery
propertyNameQuery pPropertyNameHint_ =
  PropertyNameQuery' {_pnqPropertyNameHint = pPropertyNameHint_}

-- | Text that begins a property's name.
pnqPropertyNameHint :: Lens' PropertyNameQuery Text
pnqPropertyNameHint = lens _pnqPropertyNameHint (\s a -> s {_pnqPropertyNameHint = a})

instance Hashable PropertyNameQuery

instance NFData PropertyNameQuery

instance ToJSON PropertyNameQuery where
  toJSON PropertyNameQuery' {..} =
    object
      (catMaybes [Just ("PropertyNameHint" .= _pnqPropertyNameHint)])
