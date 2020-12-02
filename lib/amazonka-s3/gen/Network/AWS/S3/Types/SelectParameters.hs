{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExpressionType
import Network.AWS.S3.Types.InputSerialization
import Network.AWS.S3.Types.OutputSerialization

-- | Describes the parameters for Select job types.
--
--
--
-- /See:/ 'selectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { _spInputSerialization ::
      !InputSerialization,
    _spExpressionType :: !ExpressionType,
    _spExpression :: !Text,
    _spOutputSerialization :: !OutputSerialization
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spInputSerialization' - Describes the serialization format of the object.
--
-- * 'spExpressionType' - The type of the provided expression (for example, SQL).
--
-- * 'spExpression' - The expression that is used to query the object.
--
-- * 'spOutputSerialization' - Describes how the results of the Select job are serialized.
selectParameters ::
  -- | 'spInputSerialization'
  InputSerialization ->
  -- | 'spExpressionType'
  ExpressionType ->
  -- | 'spExpression'
  Text ->
  -- | 'spOutputSerialization'
  OutputSerialization ->
  SelectParameters
selectParameters
  pInputSerialization_
  pExpressionType_
  pExpression_
  pOutputSerialization_ =
    SelectParameters'
      { _spInputSerialization = pInputSerialization_,
        _spExpressionType = pExpressionType_,
        _spExpression = pExpression_,
        _spOutputSerialization = pOutputSerialization_
      }

-- | Describes the serialization format of the object.
spInputSerialization :: Lens' SelectParameters InputSerialization
spInputSerialization = lens _spInputSerialization (\s a -> s {_spInputSerialization = a})

-- | The type of the provided expression (for example, SQL).
spExpressionType :: Lens' SelectParameters ExpressionType
spExpressionType = lens _spExpressionType (\s a -> s {_spExpressionType = a})

-- | The expression that is used to query the object.
spExpression :: Lens' SelectParameters Text
spExpression = lens _spExpression (\s a -> s {_spExpression = a})

-- | Describes how the results of the Select job are serialized.
spOutputSerialization :: Lens' SelectParameters OutputSerialization
spOutputSerialization = lens _spOutputSerialization (\s a -> s {_spOutputSerialization = a})

instance Hashable SelectParameters

instance NFData SelectParameters

instance ToXML SelectParameters where
  toXML SelectParameters' {..} =
    mconcat
      [ "InputSerialization" @= _spInputSerialization,
        "ExpressionType" @= _spExpressionType,
        "Expression" @= _spExpression,
        "OutputSerialization" @= _spOutputSerialization
      ]
