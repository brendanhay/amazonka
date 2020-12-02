{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NestedFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NestedFilters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Filter

-- | A list of nested 'Filter' objects. A resource must satisfy the conditions of all filters to be included in the results returned from the 'Search' API.
--
--
-- For example, to filter on a training job's @InputDataConfig@ property with a specific channel name and @S3Uri@ prefix, define the following filters:
--
--     * @'{Name:"InputDataConfig.ChannelName", "Operator":"Equals", "Value":"train"}',@
--
--     * @'{Name:"InputDataConfig.DataSource.S3DataSource.S3Uri", "Operator":"Contains", "Value":"mybucket/catdata"}'@
--
--
--
--
-- /See:/ 'nestedFilters' smart constructor.
data NestedFilters = NestedFilters'
  { _nfNestedPropertyName :: !Text,
    _nfFilters :: !(List1 Filter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NestedFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfNestedPropertyName' - The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
--
-- * 'nfFilters' - A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
nestedFilters ::
  -- | 'nfNestedPropertyName'
  Text ->
  -- | 'nfFilters'
  NonEmpty Filter ->
  NestedFilters
nestedFilters pNestedPropertyName_ pFilters_ =
  NestedFilters'
    { _nfNestedPropertyName = pNestedPropertyName_,
      _nfFilters = _List1 # pFilters_
    }

-- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
nfNestedPropertyName :: Lens' NestedFilters Text
nfNestedPropertyName = lens _nfNestedPropertyName (\s a -> s {_nfNestedPropertyName = a})

-- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
nfFilters :: Lens' NestedFilters (NonEmpty Filter)
nfFilters = lens _nfFilters (\s a -> s {_nfFilters = a}) . _List1

instance Hashable NestedFilters

instance NFData NestedFilters

instance ToJSON NestedFilters where
  toJSON NestedFilters' {..} =
    object
      ( catMaybes
          [ Just ("NestedPropertyName" .= _nfNestedPropertyName),
            Just ("Filters" .= _nfFilters)
          ]
      )
