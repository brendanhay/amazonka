{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.OperationFilterName

-- | A complex type that lets you select the operations that you want to list.
--
--
--
-- /See:/ 'operationFilter' smart constructor.
data OperationFilter = OperationFilter'
  { _ofCondition ::
      !(Maybe FilterCondition),
    _ofName :: !OperationFilterName,
    _ofValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OperationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofCondition' - The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ .
--
-- * 'ofName' - Specify the operations that you want to get:     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.     * __SERVICE_ID__ : Gets operations related to specified services.     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Gets specified types of operation.     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range.
--
-- * 'ofValues' - Specify values that are applicable to the value that you specify for @Name@ :      * __NAMESPACE_ID__ : Specify one namespace ID.     * __SERVICE_ID__ : Specify one service ID.     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
operationFilter ::
  -- | 'ofName'
  OperationFilterName ->
  OperationFilter
operationFilter pName_ =
  OperationFilter'
    { _ofCondition = Nothing,
      _ofName = pName_,
      _ofValues = mempty
    }

-- | The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ .
ofCondition :: Lens' OperationFilter (Maybe FilterCondition)
ofCondition = lens _ofCondition (\s a -> s {_ofCondition = a})

-- | Specify the operations that you want to get:     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.     * __SERVICE_ID__ : Gets operations related to specified services.     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Gets specified types of operation.     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range.
ofName :: Lens' OperationFilter OperationFilterName
ofName = lens _ofName (\s a -> s {_ofName = a})

-- | Specify values that are applicable to the value that you specify for @Name@ :      * __NAMESPACE_ID__ : Specify one namespace ID.     * __SERVICE_ID__ : Specify one service ID.     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
ofValues :: Lens' OperationFilter [Text]
ofValues = lens _ofValues (\s a -> s {_ofValues = a}) . _Coerce

instance Hashable OperationFilter

instance NFData OperationFilter

instance ToJSON OperationFilter where
  toJSON OperationFilter' {..} =
    object
      ( catMaybes
          [ ("Condition" .=) <$> _ofCondition,
            Just ("Name" .= _ofName),
            Just ("Values" .= _ofValues)
          ]
      )
