{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.MathActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.MathActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that computes an arithmetic expression using the message's attributes.
--
--
--
-- /See:/ 'mathActivity' smart constructor.
data MathActivity = MathActivity'
  { _maNext :: !(Maybe Text),
    _maName :: !Text,
    _maAttribute :: !Text,
    _maMath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MathActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maNext' - The next activity in the pipeline.
--
-- * 'maName' - The name of the math activity.
--
-- * 'maAttribute' - The name of the attribute that contains the result of the math operation.
--
-- * 'maMath' - An expression that uses one or more existing attributes and must return an integer value.
mathActivity ::
  -- | 'maName'
  Text ->
  -- | 'maAttribute'
  Text ->
  -- | 'maMath'
  Text ->
  MathActivity
mathActivity pName_ pAttribute_ pMath_ =
  MathActivity'
    { _maNext = Nothing,
      _maName = pName_,
      _maAttribute = pAttribute_,
      _maMath = pMath_
    }

-- | The next activity in the pipeline.
maNext :: Lens' MathActivity (Maybe Text)
maNext = lens _maNext (\s a -> s {_maNext = a})

-- | The name of the math activity.
maName :: Lens' MathActivity Text
maName = lens _maName (\s a -> s {_maName = a})

-- | The name of the attribute that contains the result of the math operation.
maAttribute :: Lens' MathActivity Text
maAttribute = lens _maAttribute (\s a -> s {_maAttribute = a})

-- | An expression that uses one or more existing attributes and must return an integer value.
maMath :: Lens' MathActivity Text
maMath = lens _maMath (\s a -> s {_maMath = a})

instance FromJSON MathActivity where
  parseJSON =
    withObject
      "MathActivity"
      ( \x ->
          MathActivity'
            <$> (x .:? "next")
            <*> (x .: "name")
            <*> (x .: "attribute")
            <*> (x .: "math")
      )

instance Hashable MathActivity

instance NFData MathActivity

instance ToJSON MathActivity where
  toJSON MathActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _maNext,
            Just ("name" .= _maName),
            Just ("attribute" .= _maAttribute),
            Just ("math" .= _maMath)
          ]
      )
