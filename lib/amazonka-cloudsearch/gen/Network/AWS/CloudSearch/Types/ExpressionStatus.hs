{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ExpressionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ExpressionStatus where

import Network.AWS.CloudSearch.Types.Expression
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value of an @Expression@ and its current status.
--
--
--
-- /See:/ 'expressionStatus' smart constructor.
data ExpressionStatus = ExpressionStatus'
  { _esOptions ::
      !Expression,
    _esStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExpressionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esOptions' - The expression that is evaluated for sorting while processing a search request.
--
-- * 'esStatus' - Undocumented member.
expressionStatus ::
  -- | 'esOptions'
  Expression ->
  -- | 'esStatus'
  OptionStatus ->
  ExpressionStatus
expressionStatus pOptions_ pStatus_ =
  ExpressionStatus' {_esOptions = pOptions_, _esStatus = pStatus_}

-- | The expression that is evaluated for sorting while processing a search request.
esOptions :: Lens' ExpressionStatus Expression
esOptions = lens _esOptions (\s a -> s {_esOptions = a})

-- | Undocumented member.
esStatus :: Lens' ExpressionStatus OptionStatus
esStatus = lens _esStatus (\s a -> s {_esStatus = a})

instance FromXML ExpressionStatus where
  parseXML x =
    ExpressionStatus' <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable ExpressionStatus

instance NFData ExpressionStatus
