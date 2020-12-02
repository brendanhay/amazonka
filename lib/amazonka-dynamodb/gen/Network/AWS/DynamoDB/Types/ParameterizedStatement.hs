{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ParameterizedStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ParameterizedStatement where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a PartiQL statment that uses parameters.
--
--
--
-- /See:/ 'parameterizedStatement' smart constructor.
data ParameterizedStatement = ParameterizedStatement'
  { _psParameters ::
      !(Maybe (List1 AttributeValue)),
    _psStatement :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterizedStatement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psParameters' - The parameter values.
--
-- * 'psStatement' - A PartiQL statment that uses parameters.
parameterizedStatement ::
  -- | 'psStatement'
  Text ->
  ParameterizedStatement
parameterizedStatement pStatement_ =
  ParameterizedStatement'
    { _psParameters = Nothing,
      _psStatement = pStatement_
    }

-- | The parameter values.
psParameters :: Lens' ParameterizedStatement (Maybe (NonEmpty AttributeValue))
psParameters = lens _psParameters (\s a -> s {_psParameters = a}) . mapping _List1

-- | A PartiQL statment that uses parameters.
psStatement :: Lens' ParameterizedStatement Text
psStatement = lens _psStatement (\s a -> s {_psStatement = a})

instance Hashable ParameterizedStatement

instance NFData ParameterizedStatement

instance ToJSON ParameterizedStatement where
  toJSON ParameterizedStatement' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _psParameters,
            Just ("Statement" .= _psStatement)
          ]
      )
