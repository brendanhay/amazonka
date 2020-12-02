{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey

-- | Filters for the association execution.
--
--
--
-- /See:/ 'associationExecutionTargetsFilter' smart constructor.
data AssociationExecutionTargetsFilter = AssociationExecutionTargetsFilter'
  { _aetfKey ::
      !AssociationExecutionTargetsFilterKey,
    _aetfValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationExecutionTargetsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aetfKey' - The key value used in the request.
--
-- * 'aetfValue' - The value specified for the key.
associationExecutionTargetsFilter ::
  -- | 'aetfKey'
  AssociationExecutionTargetsFilterKey ->
  -- | 'aetfValue'
  Text ->
  AssociationExecutionTargetsFilter
associationExecutionTargetsFilter pKey_ pValue_ =
  AssociationExecutionTargetsFilter'
    { _aetfKey = pKey_,
      _aetfValue = pValue_
    }

-- | The key value used in the request.
aetfKey :: Lens' AssociationExecutionTargetsFilter AssociationExecutionTargetsFilterKey
aetfKey = lens _aetfKey (\s a -> s {_aetfKey = a})

-- | The value specified for the key.
aetfValue :: Lens' AssociationExecutionTargetsFilter Text
aetfValue = lens _aetfValue (\s a -> s {_aetfValue = a})

instance Hashable AssociationExecutionTargetsFilter

instance NFData AssociationExecutionTargetsFilter

instance ToJSON AssociationExecutionTargetsFilter where
  toJSON AssociationExecutionTargetsFilter' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _aetfKey), Just ("Value" .= _aetfValue)]
      )
