{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsAggregator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsFilter

-- | One or more aggregators for viewing counts of OpsItems using different dimensions such as @Source@ , @CreatedTime@ , or @Source and CreatedTime@ , to name a few.
--
--
--
-- /See:/ 'opsAggregator' smart constructor.
data OpsAggregator = OpsAggregator'
  { _oaTypeName :: !(Maybe Text),
    _oaAggregators :: !(Maybe (List1 OpsAggregator)),
    _oaValues :: !(Maybe (Map Text (Text))),
    _oaFilters :: !(Maybe (List1 OpsFilter)),
    _oaAttributeName :: !(Maybe Text),
    _oaAggregatorType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsAggregator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaTypeName' - The data type name to use for viewing counts of OpsItems.
--
-- * 'oaAggregators' - A nested aggregator for viewing counts of OpsItems.
--
-- * 'oaValues' - The aggregator value.
--
-- * 'oaFilters' - The aggregator filters.
--
-- * 'oaAttributeName' - The name of an OpsItem attribute on which to limit the count of OpsItems.
--
-- * 'oaAggregatorType' - Either a Range or Count aggregator for limiting an OpsItem summary.
opsAggregator ::
  OpsAggregator
opsAggregator =
  OpsAggregator'
    { _oaTypeName = Nothing,
      _oaAggregators = Nothing,
      _oaValues = Nothing,
      _oaFilters = Nothing,
      _oaAttributeName = Nothing,
      _oaAggregatorType = Nothing
    }

-- | The data type name to use for viewing counts of OpsItems.
oaTypeName :: Lens' OpsAggregator (Maybe Text)
oaTypeName = lens _oaTypeName (\s a -> s {_oaTypeName = a})

-- | A nested aggregator for viewing counts of OpsItems.
oaAggregators :: Lens' OpsAggregator (Maybe (NonEmpty OpsAggregator))
oaAggregators = lens _oaAggregators (\s a -> s {_oaAggregators = a}) . mapping _List1

-- | The aggregator value.
oaValues :: Lens' OpsAggregator (HashMap Text (Text))
oaValues = lens _oaValues (\s a -> s {_oaValues = a}) . _Default . _Map

-- | The aggregator filters.
oaFilters :: Lens' OpsAggregator (Maybe (NonEmpty OpsFilter))
oaFilters = lens _oaFilters (\s a -> s {_oaFilters = a}) . mapping _List1

-- | The name of an OpsItem attribute on which to limit the count of OpsItems.
oaAttributeName :: Lens' OpsAggregator (Maybe Text)
oaAttributeName = lens _oaAttributeName (\s a -> s {_oaAttributeName = a})

-- | Either a Range or Count aggregator for limiting an OpsItem summary.
oaAggregatorType :: Lens' OpsAggregator (Maybe Text)
oaAggregatorType = lens _oaAggregatorType (\s a -> s {_oaAggregatorType = a})

instance Hashable OpsAggregator

instance NFData OpsAggregator

instance ToJSON OpsAggregator where
  toJSON OpsAggregator' {..} =
    object
      ( catMaybes
          [ ("TypeName" .=) <$> _oaTypeName,
            ("Aggregators" .=) <$> _oaAggregators,
            ("Values" .=) <$> _oaValues,
            ("Filters" .=) <$> _oaFilters,
            ("AttributeName" .=) <$> _oaAttributeName,
            ("AggregatorType" .=) <$> _oaAggregatorType
          ]
      )
