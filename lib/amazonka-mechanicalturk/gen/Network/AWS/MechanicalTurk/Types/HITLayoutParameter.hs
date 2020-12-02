{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITLayoutParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITLayoutParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The HITLayoutParameter data structure defines parameter values used with a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project template used to provide Human Intelligence Task (HIT) question data for CreateHIT.
--
--
--
-- /See:/ 'hITLayoutParameter' smart constructor.
data HITLayoutParameter = HITLayoutParameter'
  { _hitlpName :: !Text,
    _hitlpValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HITLayoutParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hitlpName' - The name of the parameter in the HITLayout.
--
-- * 'hitlpValue' - The value substituted for the parameter referenced in the HITLayout.
hITLayoutParameter ::
  -- | 'hitlpName'
  Text ->
  -- | 'hitlpValue'
  Text ->
  HITLayoutParameter
hITLayoutParameter pName_ pValue_ =
  HITLayoutParameter' {_hitlpName = pName_, _hitlpValue = pValue_}

-- | The name of the parameter in the HITLayout.
hitlpName :: Lens' HITLayoutParameter Text
hitlpName = lens _hitlpName (\s a -> s {_hitlpName = a})

-- | The value substituted for the parameter referenced in the HITLayout.
hitlpValue :: Lens' HITLayoutParameter Text
hitlpValue = lens _hitlpValue (\s a -> s {_hitlpValue = a})

instance Hashable HITLayoutParameter

instance NFData HITLayoutParameter

instance ToJSON HITLayoutParameter where
  toJSON HITLayoutParameter' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _hitlpName), Just ("Value" .= _hitlpValue)]
      )
