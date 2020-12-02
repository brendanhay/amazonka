{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.BudgetDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.BudgetDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a budget.
--
--
--
-- /See:/ 'budgetDetail' smart constructor.
newtype BudgetDetail = BudgetDetail' {_bdBudgetName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BudgetDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBudgetName' - Name of the associated budget.
budgetDetail ::
  BudgetDetail
budgetDetail = BudgetDetail' {_bdBudgetName = Nothing}

-- | Name of the associated budget.
bdBudgetName :: Lens' BudgetDetail (Maybe Text)
bdBudgetName = lens _bdBudgetName (\s a -> s {_bdBudgetName = a})

instance FromJSON BudgetDetail where
  parseJSON =
    withObject
      "BudgetDetail"
      (\x -> BudgetDetail' <$> (x .:? "BudgetName"))

instance Hashable BudgetDetail

instance NFData BudgetDetail
