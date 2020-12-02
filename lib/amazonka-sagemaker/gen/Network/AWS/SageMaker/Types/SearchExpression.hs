{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchExpression where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.BooleanOperator
import Network.AWS.SageMaker.Types.Filter
import Network.AWS.SageMaker.Types.NestedFilters

-- | A multi-expression that searches for the specified resource or resources in a search. All resource objects that satisfy the expression's condition are included in the search results. You must specify at least one subexpression, filter, or nested filter. A @SearchExpression@ can contain up to twenty elements.
--
--
-- A @SearchExpression@ contains the following components:
--
--     * A list of @Filter@ objects. Each filter defines a simple Boolean expression comprised of a resource property name, Boolean operator, and value.
--
--     * A list of @NestedFilter@ objects. Each nested filter defines a list of Boolean expressions using a list of resource properties. A nested filter is satisfied if a single object in the list satisfies all Boolean expressions.
--
--     * A list of @SearchExpression@ objects. A search expression object can be nested in a list of search expression objects.
--
--     * A Boolean operator: @And@ or @Or@ .
--
--
--
--
-- /See:/ 'searchExpression' smart constructor.
data SearchExpression = SearchExpression'
  { _seSubExpressions ::
      !(Maybe (List1 SearchExpression)),
    _seOperator :: !(Maybe BooleanOperator),
    _seFilters :: !(Maybe (List1 Filter)),
    _seNestedFilters :: !(Maybe (List1 NestedFilters))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seSubExpressions' - A list of search expression objects.
--
-- * 'seOperator' - A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
--
-- * 'seFilters' - A list of filter objects.
--
-- * 'seNestedFilters' - A list of nested filter objects.
searchExpression ::
  SearchExpression
searchExpression =
  SearchExpression'
    { _seSubExpressions = Nothing,
      _seOperator = Nothing,
      _seFilters = Nothing,
      _seNestedFilters = Nothing
    }

-- | A list of search expression objects.
seSubExpressions :: Lens' SearchExpression (Maybe (NonEmpty SearchExpression))
seSubExpressions = lens _seSubExpressions (\s a -> s {_seSubExpressions = a}) . mapping _List1

-- | A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
seOperator :: Lens' SearchExpression (Maybe BooleanOperator)
seOperator = lens _seOperator (\s a -> s {_seOperator = a})

-- | A list of filter objects.
seFilters :: Lens' SearchExpression (Maybe (NonEmpty Filter))
seFilters = lens _seFilters (\s a -> s {_seFilters = a}) . mapping _List1

-- | A list of nested filter objects.
seNestedFilters :: Lens' SearchExpression (Maybe (NonEmpty NestedFilters))
seNestedFilters = lens _seNestedFilters (\s a -> s {_seNestedFilters = a}) . mapping _List1

instance Hashable SearchExpression

instance NFData SearchExpression

instance ToJSON SearchExpression where
  toJSON SearchExpression' {..} =
    object
      ( catMaybes
          [ ("SubExpressions" .=) <$> _seSubExpressions,
            ("Operator" .=) <$> _seOperator,
            ("Filters" .=) <$> _seFilters,
            ("NestedFilters" .=) <$> _seNestedFilters
          ]
      )
