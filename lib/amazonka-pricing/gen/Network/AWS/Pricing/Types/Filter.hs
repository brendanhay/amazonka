{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Pricing.Types.FilterType

-- | The constraints that you want all returned products to match.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fType :: !FilterType,
    _fField :: !Text,
    _fValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fType' - The type of filter that you want to use. Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
--
-- * 'fField' - The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields. Valid values include: @ServiceCode@ , and all attribute names For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
--
-- * 'fValue' - The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
filter' ::
  -- | 'fType'
  FilterType ->
  -- | 'fField'
  Text ->
  -- | 'fValue'
  Text ->
  Filter
filter' pType_ pField_ pValue_ =
  Filter' {_fType = pType_, _fField = pField_, _fValue = pValue_}

-- | The type of filter that you want to use. Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
fType :: Lens' Filter FilterType
fType = lens _fType (\s a -> s {_fType = a})

-- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields. Valid values include: @ServiceCode@ , and all attribute names For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
fField :: Lens' Filter Text
fField = lens _fField (\s a -> s {_fField = a})

-- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
fValue :: Lens' Filter Text
fValue = lens _fValue (\s a -> s {_fValue = a})

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      ( catMaybes
          [ Just ("Type" .= _fType),
            Just ("Field" .= _fField),
            Just ("Value" .= _fValue)
          ]
      )
