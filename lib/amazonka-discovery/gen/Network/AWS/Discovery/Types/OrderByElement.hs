{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.OrderByElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.OrderByElement where

import Network.AWS.Discovery.Types.OrderString
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A field and direction for ordered output.
--
--
--
-- /See:/ 'orderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { _obeSortOrder ::
      !(Maybe OrderString),
    _obeFieldName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrderByElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'obeSortOrder' - Ordering direction.
--
-- * 'obeFieldName' - The field on which to order.
orderByElement ::
  -- | 'obeFieldName'
  Text ->
  OrderByElement
orderByElement pFieldName_ =
  OrderByElement'
    { _obeSortOrder = Nothing,
      _obeFieldName = pFieldName_
    }

-- | Ordering direction.
obeSortOrder :: Lens' OrderByElement (Maybe OrderString)
obeSortOrder = lens _obeSortOrder (\s a -> s {_obeSortOrder = a})

-- | The field on which to order.
obeFieldName :: Lens' OrderByElement Text
obeFieldName = lens _obeFieldName (\s a -> s {_obeFieldName = a})

instance Hashable OrderByElement

instance NFData OrderByElement

instance ToJSON OrderByElement where
  toJSON OrderByElement' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _obeSortOrder,
            Just ("fieldName" .= _obeFieldName)
          ]
      )
