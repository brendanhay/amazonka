{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Predicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Predicate where

import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.Logical
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the predicate of the trigger, which determines when it fires.
--
--
--
-- /See:/ 'predicate' smart constructor.
data Predicate = Predicate'
  { _pLogical :: !(Maybe Logical),
    _pConditions :: !(Maybe [Condition])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Predicate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLogical' - An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
--
-- * 'pConditions' - A list of the conditions that determine when the trigger will fire.
predicate ::
  Predicate
predicate = Predicate' {_pLogical = Nothing, _pConditions = Nothing}

-- | An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
pLogical :: Lens' Predicate (Maybe Logical)
pLogical = lens _pLogical (\s a -> s {_pLogical = a})

-- | A list of the conditions that determine when the trigger will fire.
pConditions :: Lens' Predicate [Condition]
pConditions = lens _pConditions (\s a -> s {_pConditions = a}) . _Default . _Coerce

instance FromJSON Predicate where
  parseJSON =
    withObject
      "Predicate"
      ( \x ->
          Predicate'
            <$> (x .:? "Logical") <*> (x .:? "Conditions" .!= mempty)
      )

instance Hashable Predicate

instance NFData Predicate

instance ToJSON Predicate where
  toJSON Predicate' {..} =
    object
      ( catMaybes
          [("Logical" .=) <$> _pLogical, ("Conditions" .=) <$> _pConditions]
      )
