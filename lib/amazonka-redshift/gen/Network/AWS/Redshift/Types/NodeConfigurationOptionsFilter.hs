{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
import Network.AWS.Redshift.Types.OperatorType

-- | A set of elements to filter the returned node configurations.
--
--
--
-- /See:/ 'nodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { _ncofValues ::
      !(Maybe [Text]),
    _ncofOperator ::
      !(Maybe OperatorType),
    _ncofName ::
      !( Maybe
           NodeConfigurationOptionsFilterName
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeConfigurationOptionsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncofValues' - List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
--
-- * 'ncofOperator' - The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
--
-- * 'ncofName' - The name of the element to filter.
nodeConfigurationOptionsFilter ::
  NodeConfigurationOptionsFilter
nodeConfigurationOptionsFilter =
  NodeConfigurationOptionsFilter'
    { _ncofValues = Nothing,
      _ncofOperator = Nothing,
      _ncofName = Nothing
    }

-- | List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
ncofValues :: Lens' NodeConfigurationOptionsFilter [Text]
ncofValues = lens _ncofValues (\s a -> s {_ncofValues = a}) . _Default . _Coerce

-- | The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
ncofOperator :: Lens' NodeConfigurationOptionsFilter (Maybe OperatorType)
ncofOperator = lens _ncofOperator (\s a -> s {_ncofOperator = a})

-- | The name of the element to filter.
ncofName :: Lens' NodeConfigurationOptionsFilter (Maybe NodeConfigurationOptionsFilterName)
ncofName = lens _ncofName (\s a -> s {_ncofName = a})

instance Hashable NodeConfigurationOptionsFilter

instance NFData NodeConfigurationOptionsFilter

instance ToQuery NodeConfigurationOptionsFilter where
  toQuery NodeConfigurationOptionsFilter' {..} =
    mconcat
      [ "Value" =: toQuery (toQueryList "item" <$> _ncofValues),
        "Operator" =: _ncofOperator,
        "Name" =: _ncofName
      ]
