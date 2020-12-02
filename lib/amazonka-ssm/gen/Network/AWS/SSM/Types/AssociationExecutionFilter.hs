{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationExecutionFilterKey
import Network.AWS.SSM.Types.AssociationFilterOperatorType

-- | Filters used in the request.
--
--
--
-- /See:/ 'associationExecutionFilter' smart constructor.
data AssociationExecutionFilter = AssociationExecutionFilter'
  { _aefKey ::
      !AssociationExecutionFilterKey,
    _aefValue :: !Text,
    _aefType ::
      !AssociationFilterOperatorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aefKey' - The key value used in the request.
--
-- * 'aefValue' - The value specified for the key.
--
-- * 'aefType' - The filter type specified in the request.
associationExecutionFilter ::
  -- | 'aefKey'
  AssociationExecutionFilterKey ->
  -- | 'aefValue'
  Text ->
  -- | 'aefType'
  AssociationFilterOperatorType ->
  AssociationExecutionFilter
associationExecutionFilter pKey_ pValue_ pType_ =
  AssociationExecutionFilter'
    { _aefKey = pKey_,
      _aefValue = pValue_,
      _aefType = pType_
    }

-- | The key value used in the request.
aefKey :: Lens' AssociationExecutionFilter AssociationExecutionFilterKey
aefKey = lens _aefKey (\s a -> s {_aefKey = a})

-- | The value specified for the key.
aefValue :: Lens' AssociationExecutionFilter Text
aefValue = lens _aefValue (\s a -> s {_aefValue = a})

-- | The filter type specified in the request.
aefType :: Lens' AssociationExecutionFilter AssociationFilterOperatorType
aefType = lens _aefType (\s a -> s {_aefType = a})

instance Hashable AssociationExecutionFilter

instance NFData AssociationExecutionFilter

instance ToJSON AssociationExecutionFilter where
  toJSON AssociationExecutionFilter' {..} =
    object
      ( catMaybes
          [ Just ("Key" .= _aefKey),
            Just ("Value" .= _aefValue),
            Just ("Type" .= _aefType)
          ]
      )
