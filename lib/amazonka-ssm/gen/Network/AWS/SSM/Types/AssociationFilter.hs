{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationFilterKey

-- | Describes a filter.
--
--
--
-- /See:/ 'associationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
  { _afKey ::
      !AssociationFilterKey,
    _afValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afKey' - The name of the filter.
--
-- * 'afValue' - The filter value.
associationFilter ::
  -- | 'afKey'
  AssociationFilterKey ->
  -- | 'afValue'
  Text ->
  AssociationFilter
associationFilter pKey_ pValue_ =
  AssociationFilter' {_afKey = pKey_, _afValue = pValue_}

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\s a -> s {_afKey = a})

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\s a -> s {_afValue = a})

instance Hashable AssociationFilter

instance NFData AssociationFilter

instance ToJSON AssociationFilter where
  toJSON AssociationFilter' {..} =
    object
      (catMaybes [Just ("key" .= _afKey), Just ("value" .= _afValue)])
