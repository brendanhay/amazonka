{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.SelectParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.SelectParameters where

import Network.AWS.Glacier.Types.ExpressionType
import Network.AWS.Glacier.Types.InputSerialization
import Network.AWS.Glacier.Types.OutputSerialization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the parameters used for a select.
--
--
--
-- /See:/ 'selectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { _spExpressionType ::
      !(Maybe ExpressionType),
    _spOutputSerialization :: !(Maybe OutputSerialization),
    _spExpression :: !(Maybe Text),
    _spInputSerialization :: !(Maybe InputSerialization)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spExpressionType' - The type of the provided expression, for example @SQL@ .
--
-- * 'spOutputSerialization' - Describes how the results of the select job are serialized.
--
-- * 'spExpression' - The expression that is used to select the object.
--
-- * 'spInputSerialization' - Describes the serialization format of the object.
selectParameters ::
  SelectParameters
selectParameters =
  SelectParameters'
    { _spExpressionType = Nothing,
      _spOutputSerialization = Nothing,
      _spExpression = Nothing,
      _spInputSerialization = Nothing
    }

-- | The type of the provided expression, for example @SQL@ .
spExpressionType :: Lens' SelectParameters (Maybe ExpressionType)
spExpressionType = lens _spExpressionType (\s a -> s {_spExpressionType = a})

-- | Describes how the results of the select job are serialized.
spOutputSerialization :: Lens' SelectParameters (Maybe OutputSerialization)
spOutputSerialization = lens _spOutputSerialization (\s a -> s {_spOutputSerialization = a})

-- | The expression that is used to select the object.
spExpression :: Lens' SelectParameters (Maybe Text)
spExpression = lens _spExpression (\s a -> s {_spExpression = a})

-- | Describes the serialization format of the object.
spInputSerialization :: Lens' SelectParameters (Maybe InputSerialization)
spInputSerialization = lens _spInputSerialization (\s a -> s {_spInputSerialization = a})

instance FromJSON SelectParameters where
  parseJSON =
    withObject
      "SelectParameters"
      ( \x ->
          SelectParameters'
            <$> (x .:? "ExpressionType")
            <*> (x .:? "OutputSerialization")
            <*> (x .:? "Expression")
            <*> (x .:? "InputSerialization")
      )

instance Hashable SelectParameters

instance NFData SelectParameters

instance ToJSON SelectParameters where
  toJSON SelectParameters' {..} =
    object
      ( catMaybes
          [ ("ExpressionType" .=) <$> _spExpressionType,
            ("OutputSerialization" .=) <$> _spOutputSerialization,
            ("Expression" .=) <$> _spExpression,
            ("InputSerialization" .=) <$> _spInputSerialization
          ]
      )
