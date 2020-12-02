{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.PolicyParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.PolicyParameter where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import Network.AWS.Prelude

-- | Name of the parameter from the Review policy.
--
--
--
-- /See:/ 'policyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { _ppValues ::
      !(Maybe [Text]),
    _ppMapEntries :: !(Maybe [ParameterMapEntry]),
    _ppKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppValues' - The list of values of the Parameter
--
-- * 'ppMapEntries' - List of ParameterMapEntry objects.
--
-- * 'ppKey' - Name of the parameter from the list of Review Polices.
policyParameter ::
  PolicyParameter
policyParameter =
  PolicyParameter'
    { _ppValues = Nothing,
      _ppMapEntries = Nothing,
      _ppKey = Nothing
    }

-- | The list of values of the Parameter
ppValues :: Lens' PolicyParameter [Text]
ppValues = lens _ppValues (\s a -> s {_ppValues = a}) . _Default . _Coerce

-- | List of ParameterMapEntry objects.
ppMapEntries :: Lens' PolicyParameter [ParameterMapEntry]
ppMapEntries = lens _ppMapEntries (\s a -> s {_ppMapEntries = a}) . _Default . _Coerce

-- | Name of the parameter from the list of Review Polices.
ppKey :: Lens' PolicyParameter (Maybe Text)
ppKey = lens _ppKey (\s a -> s {_ppKey = a})

instance FromJSON PolicyParameter where
  parseJSON =
    withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            <$> (x .:? "Values" .!= mempty)
            <*> (x .:? "MapEntries" .!= mempty)
            <*> (x .:? "Key")
      )

instance Hashable PolicyParameter

instance NFData PolicyParameter

instance ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    object
      ( catMaybes
          [ ("Values" .=) <$> _ppValues,
            ("MapEntries" .=) <$> _ppMapEntries,
            ("Key" .=) <$> _ppKey
          ]
      )
