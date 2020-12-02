{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Expression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Expression where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A named expression that can be evaluated at search time. Can be used to sort the search results, define other expressions, or return computed information in the search results.
--
--
--
-- /See:/ 'expression' smart constructor.
data Expression = Expression'
  { _eExpressionName :: !Text,
    _eExpressionValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eExpressionName' - Undocumented member.
--
-- * 'eExpressionValue' - Undocumented member.
expression ::
  -- | 'eExpressionName'
  Text ->
  -- | 'eExpressionValue'
  Text ->
  Expression
expression pExpressionName_ pExpressionValue_ =
  Expression'
    { _eExpressionName = pExpressionName_,
      _eExpressionValue = pExpressionValue_
    }

-- | Undocumented member.
eExpressionName :: Lens' Expression Text
eExpressionName = lens _eExpressionName (\s a -> s {_eExpressionName = a})

-- | Undocumented member.
eExpressionValue :: Lens' Expression Text
eExpressionValue = lens _eExpressionValue (\s a -> s {_eExpressionValue = a})

instance FromXML Expression where
  parseXML x =
    Expression'
      <$> (x .@ "ExpressionName") <*> (x .@ "ExpressionValue")

instance Hashable Expression

instance NFData Expression

instance ToQuery Expression where
  toQuery Expression' {..} =
    mconcat
      [ "ExpressionName" =: _eExpressionName,
        "ExpressionValue" =: _eExpressionValue
      ]
