{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Expression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Expression where

import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use @Expression@ to filter by cost or by usage. There are two patterns:
--
--
--     * Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for @REGION==us-east-1 OR REGION==us-west-1@ . For @GetRightsizingRecommendation@ , the Region is a full name (for example, @REGION==US East (N. Virginia)@ . The @Expression@ example looks like:
--
-- @{ "Dimensions": { "Key": "REGION", "Values": [ "us-east-1", “us-west-1” ] } }@
--
-- The list of dimension values are OR'd together to retrieve cost or usage data. You can create @Expression@ and @DimensionValues@ objects using either @with*@ methods or @set*@ methods in multiple lines.
--
--     * Compound dimension values with logical operations - You can use multiple @Expression@ types and the logical operators @AND/OR/NOT@ to create a list of one or more @Expression@ objects. This allows you to filter on more advanced options. For example, you can filter on @((REGION == us-east-1 OR REGION == us-west-1) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)@ . The @Expression@ for that looks like this:
--
-- @{ "And": [ {"Or": [ {"Dimensions": { "Key": "REGION", "Values": [ "us-east-1", "us-west-1" ] }}, {"Tags": { "Key": "TagName", "Values": ["Value1"] } } ]}, {"Not": {"Dimensions": { "Key": "USAGE_TYPE", "Values": ["DataTransfer"] }}} ] } @
--
-- @{ "And": [ ... ], "DimensionValues": { "Dimension": "USAGE_TYPE", "Values": [ "DataTransfer" ] } } @
--
--
--
--
-- /See:/ 'expression' smart constructor.
data Expression = Expression'
  { _eNot :: !(Maybe Expression),
    _eAnd :: !(Maybe [Expression]),
    _eOr :: !(Maybe [Expression]),
    _eCostCategories :: !(Maybe CostCategoryValues),
    _eDimensions :: !(Maybe DimensionValues),
    _eTags :: !(Maybe TagValues)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eNot' - Return results that don't match a @Dimension@ object.
--
-- * 'eAnd' - Return results that match both @Dimension@ objects.
--
-- * 'eOr' - Return results that match either @Dimension@ object.
--
-- * 'eCostCategories' - The filter based on @CostCategory@ values.
--
-- * 'eDimensions' - The specific @Dimension@ to use for @Expression@ .
--
-- * 'eTags' - The specific @Tag@ to use for @Expression@ .
expression ::
  Expression
expression =
  Expression'
    { _eNot = Nothing,
      _eAnd = Nothing,
      _eOr = Nothing,
      _eCostCategories = Nothing,
      _eDimensions = Nothing,
      _eTags = Nothing
    }

-- | Return results that don't match a @Dimension@ object.
eNot :: Lens' Expression (Maybe Expression)
eNot = lens _eNot (\s a -> s {_eNot = a})

-- | Return results that match both @Dimension@ objects.
eAnd :: Lens' Expression [Expression]
eAnd = lens _eAnd (\s a -> s {_eAnd = a}) . _Default . _Coerce

-- | Return results that match either @Dimension@ object.
eOr :: Lens' Expression [Expression]
eOr = lens _eOr (\s a -> s {_eOr = a}) . _Default . _Coerce

-- | The filter based on @CostCategory@ values.
eCostCategories :: Lens' Expression (Maybe CostCategoryValues)
eCostCategories = lens _eCostCategories (\s a -> s {_eCostCategories = a})

-- | The specific @Dimension@ to use for @Expression@ .
eDimensions :: Lens' Expression (Maybe DimensionValues)
eDimensions = lens _eDimensions (\s a -> s {_eDimensions = a})

-- | The specific @Tag@ to use for @Expression@ .
eTags :: Lens' Expression (Maybe TagValues)
eTags = lens _eTags (\s a -> s {_eTags = a})

instance FromJSON Expression where
  parseJSON =
    withObject
      "Expression"
      ( \x ->
          Expression'
            <$> (x .:? "Not")
            <*> (x .:? "And" .!= mempty)
            <*> (x .:? "Or" .!= mempty)
            <*> (x .:? "CostCategories")
            <*> (x .:? "Dimensions")
            <*> (x .:? "Tags")
      )

instance Hashable Expression

instance NFData Expression

instance ToJSON Expression where
  toJSON Expression' {..} =
    object
      ( catMaybes
          [ ("Not" .=) <$> _eNot,
            ("And" .=) <$> _eAnd,
            ("Or" .=) <$> _eOr,
            ("CostCategories" .=) <$> _eCostCategories,
            ("Dimensions" .=) <$> _eDimensions,
            ("Tags" .=) <$> _eTags
          ]
      )
