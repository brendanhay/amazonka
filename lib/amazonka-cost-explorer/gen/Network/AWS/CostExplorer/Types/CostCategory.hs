{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategory where

import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure of Cost Categories. This includes detailed metadata and the set of rules for the @CostCategory@ object.
--
--
--
-- /See:/ 'costCategory' smart constructor.
data CostCategory = CostCategory'
  { _ccProcessingStatus ::
      !(Maybe [CostCategoryProcessingStatus]),
    _ccEffectiveEnd :: !(Maybe Text),
    _ccCostCategoryARN :: !Text,
    _ccEffectiveStart :: !Text,
    _ccName :: !Text,
    _ccRuleVersion :: !CostCategoryRuleVersion,
    _ccRules :: !(List1 CostCategoryRule)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CostCategory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccProcessingStatus' - The list of processing statuses for Cost Management products for a specific cost category.
--
-- * 'ccEffectiveEnd' - The Cost Category's effective end date.
--
-- * 'ccCostCategoryARN' - The unique identifier for your Cost Category.
--
-- * 'ccEffectiveStart' - The Cost Category's effective start date.
--
-- * 'ccName' - Undocumented member.
--
-- * 'ccRuleVersion' - Undocumented member.
--
-- * 'ccRules' - Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
costCategory ::
  -- | 'ccCostCategoryARN'
  Text ->
  -- | 'ccEffectiveStart'
  Text ->
  -- | 'ccName'
  Text ->
  -- | 'ccRuleVersion'
  CostCategoryRuleVersion ->
  -- | 'ccRules'
  NonEmpty CostCategoryRule ->
  CostCategory
costCategory
  pCostCategoryARN_
  pEffectiveStart_
  pName_
  pRuleVersion_
  pRules_ =
    CostCategory'
      { _ccProcessingStatus = Nothing,
        _ccEffectiveEnd = Nothing,
        _ccCostCategoryARN = pCostCategoryARN_,
        _ccEffectiveStart = pEffectiveStart_,
        _ccName = pName_,
        _ccRuleVersion = pRuleVersion_,
        _ccRules = _List1 # pRules_
      }

-- | The list of processing statuses for Cost Management products for a specific cost category.
ccProcessingStatus :: Lens' CostCategory [CostCategoryProcessingStatus]
ccProcessingStatus = lens _ccProcessingStatus (\s a -> s {_ccProcessingStatus = a}) . _Default . _Coerce

-- | The Cost Category's effective end date.
ccEffectiveEnd :: Lens' CostCategory (Maybe Text)
ccEffectiveEnd = lens _ccEffectiveEnd (\s a -> s {_ccEffectiveEnd = a})

-- | The unique identifier for your Cost Category.
ccCostCategoryARN :: Lens' CostCategory Text
ccCostCategoryARN = lens _ccCostCategoryARN (\s a -> s {_ccCostCategoryARN = a})

-- | The Cost Category's effective start date.
ccEffectiveStart :: Lens' CostCategory Text
ccEffectiveStart = lens _ccEffectiveStart (\s a -> s {_ccEffectiveStart = a})

-- | Undocumented member.
ccName :: Lens' CostCategory Text
ccName = lens _ccName (\s a -> s {_ccName = a})

-- | Undocumented member.
ccRuleVersion :: Lens' CostCategory CostCategoryRuleVersion
ccRuleVersion = lens _ccRuleVersion (\s a -> s {_ccRuleVersion = a})

-- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
ccRules :: Lens' CostCategory (NonEmpty CostCategoryRule)
ccRules = lens _ccRules (\s a -> s {_ccRules = a}) . _List1

instance FromJSON CostCategory where
  parseJSON =
    withObject
      "CostCategory"
      ( \x ->
          CostCategory'
            <$> (x .:? "ProcessingStatus" .!= mempty)
            <*> (x .:? "EffectiveEnd")
            <*> (x .: "CostCategoryArn")
            <*> (x .: "EffectiveStart")
            <*> (x .: "Name")
            <*> (x .: "RuleVersion")
            <*> (x .: "Rules")
      )

instance Hashable CostCategory

instance NFData CostCategory
