{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.OperationFilterName

-- | A complex type that lets you select the operations that you want to
-- list.
--
-- /See:/ 'newOperationFilter' smart constructor.
data OperationFilter = OperationFilter'
  { -- | The operator that you want to use to determine whether an operation
    -- matches the specified value. Valid values for condition include:
    --
    -- -   @EQ@: When you specify @EQ@ for the condition, you can specify only
    --     one value. @EQ@ is supported for @NAMESPACE_ID@, @SERVICE_ID@,
    --     @STATUS@, and @TYPE@. @EQ@ is the default condition and can be
    --     omitted.
    --
    -- -   @IN@: When you specify @IN@ for the condition, you can specify a
    --     list of one or more values. @IN@ is supported for @STATUS@ and
    --     @TYPE@. An operation must match one of the specified values to be
    --     returned in the response.
    --
    -- -   @BETWEEN@: Specify a start date and an end date in Unix date\/time
    --     format and Coordinated Universal Time (UTC). The start date must be
    --     the first value. @BETWEEN@ is supported for @UPDATE_DATE@.
    condition :: Prelude.Maybe FilterCondition,
    -- | Specify the operations that you want to get:
    --
    -- -   __NAMESPACE_ID__: Gets operations related to specified namespaces.
    --
    -- -   __SERVICE_ID__: Gets operations related to specified services.
    --
    -- -   __STATUS__: Gets operations based on the status of the operations:
    --     @SUBMITTED@, @PENDING@, @SUCCEED@, or @FAIL@.
    --
    -- -   __TYPE__: Gets specified types of operation.
    --
    -- -   __UPDATE_DATE__: Gets operations that changed status during a
    --     specified date\/time range.
    name :: OperationFilterName,
    -- | Specify values that are applicable to the value that you specify for
    -- @Name@:
    --
    -- -   __NAMESPACE_ID__: Specify one namespace ID.
    --
    -- -   __SERVICE_ID__: Specify one service ID.
    --
    -- -   __STATUS__: Specify one or more statuses: @SUBMITTED@, @PENDING@,
    --     @SUCCEED@, or @FAIL@.
    --
    -- -   __TYPE__: Specify one or more of the following types:
    --     @CREATE_NAMESPACE@, @DELETE_NAMESPACE@, @UPDATE_SERVICE@,
    --     @REGISTER_INSTANCE@, or @DEREGISTER_INSTANCE@.
    --
    -- -   __UPDATE_DATE__: Specify a start date and an end date in Unix
    --     date\/time format and Coordinated Universal Time (UTC). The start
    --     date must be the first value.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OperationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'operationFilter_condition' - The operator that you want to use to determine whether an operation
-- matches the specified value. Valid values for condition include:
--
-- -   @EQ@: When you specify @EQ@ for the condition, you can specify only
--     one value. @EQ@ is supported for @NAMESPACE_ID@, @SERVICE_ID@,
--     @STATUS@, and @TYPE@. @EQ@ is the default condition and can be
--     omitted.
--
-- -   @IN@: When you specify @IN@ for the condition, you can specify a
--     list of one or more values. @IN@ is supported for @STATUS@ and
--     @TYPE@. An operation must match one of the specified values to be
--     returned in the response.
--
-- -   @BETWEEN@: Specify a start date and an end date in Unix date\/time
--     format and Coordinated Universal Time (UTC). The start date must be
--     the first value. @BETWEEN@ is supported for @UPDATE_DATE@.
--
-- 'name', 'operationFilter_name' - Specify the operations that you want to get:
--
-- -   __NAMESPACE_ID__: Gets operations related to specified namespaces.
--
-- -   __SERVICE_ID__: Gets operations related to specified services.
--
-- -   __STATUS__: Gets operations based on the status of the operations:
--     @SUBMITTED@, @PENDING@, @SUCCEED@, or @FAIL@.
--
-- -   __TYPE__: Gets specified types of operation.
--
-- -   __UPDATE_DATE__: Gets operations that changed status during a
--     specified date\/time range.
--
-- 'values', 'operationFilter_values' - Specify values that are applicable to the value that you specify for
-- @Name@:
--
-- -   __NAMESPACE_ID__: Specify one namespace ID.
--
-- -   __SERVICE_ID__: Specify one service ID.
--
-- -   __STATUS__: Specify one or more statuses: @SUBMITTED@, @PENDING@,
--     @SUCCEED@, or @FAIL@.
--
-- -   __TYPE__: Specify one or more of the following types:
--     @CREATE_NAMESPACE@, @DELETE_NAMESPACE@, @UPDATE_SERVICE@,
--     @REGISTER_INSTANCE@, or @DEREGISTER_INSTANCE@.
--
-- -   __UPDATE_DATE__: Specify a start date and an end date in Unix
--     date\/time format and Coordinated Universal Time (UTC). The start
--     date must be the first value.
newOperationFilter ::
  -- | 'name'
  OperationFilterName ->
  OperationFilter
newOperationFilter pName_ =
  OperationFilter'
    { condition = Prelude.Nothing,
      name = pName_,
      values = Prelude.mempty
    }

-- | The operator that you want to use to determine whether an operation
-- matches the specified value. Valid values for condition include:
--
-- -   @EQ@: When you specify @EQ@ for the condition, you can specify only
--     one value. @EQ@ is supported for @NAMESPACE_ID@, @SERVICE_ID@,
--     @STATUS@, and @TYPE@. @EQ@ is the default condition and can be
--     omitted.
--
-- -   @IN@: When you specify @IN@ for the condition, you can specify a
--     list of one or more values. @IN@ is supported for @STATUS@ and
--     @TYPE@. An operation must match one of the specified values to be
--     returned in the response.
--
-- -   @BETWEEN@: Specify a start date and an end date in Unix date\/time
--     format and Coordinated Universal Time (UTC). The start date must be
--     the first value. @BETWEEN@ is supported for @UPDATE_DATE@.
operationFilter_condition :: Lens.Lens' OperationFilter (Prelude.Maybe FilterCondition)
operationFilter_condition = Lens.lens (\OperationFilter' {condition} -> condition) (\s@OperationFilter' {} a -> s {condition = a} :: OperationFilter)

-- | Specify the operations that you want to get:
--
-- -   __NAMESPACE_ID__: Gets operations related to specified namespaces.
--
-- -   __SERVICE_ID__: Gets operations related to specified services.
--
-- -   __STATUS__: Gets operations based on the status of the operations:
--     @SUBMITTED@, @PENDING@, @SUCCEED@, or @FAIL@.
--
-- -   __TYPE__: Gets specified types of operation.
--
-- -   __UPDATE_DATE__: Gets operations that changed status during a
--     specified date\/time range.
operationFilter_name :: Lens.Lens' OperationFilter OperationFilterName
operationFilter_name = Lens.lens (\OperationFilter' {name} -> name) (\s@OperationFilter' {} a -> s {name = a} :: OperationFilter)

-- | Specify values that are applicable to the value that you specify for
-- @Name@:
--
-- -   __NAMESPACE_ID__: Specify one namespace ID.
--
-- -   __SERVICE_ID__: Specify one service ID.
--
-- -   __STATUS__: Specify one or more statuses: @SUBMITTED@, @PENDING@,
--     @SUCCEED@, or @FAIL@.
--
-- -   __TYPE__: Specify one or more of the following types:
--     @CREATE_NAMESPACE@, @DELETE_NAMESPACE@, @UPDATE_SERVICE@,
--     @REGISTER_INSTANCE@, or @DEREGISTER_INSTANCE@.
--
-- -   __UPDATE_DATE__: Specify a start date and an end date in Unix
--     date\/time format and Coordinated Universal Time (UTC). The start
--     date must be the first value.
operationFilter_values :: Lens.Lens' OperationFilter [Prelude.Text]
operationFilter_values = Lens.lens (\OperationFilter' {values} -> values) (\s@OperationFilter' {} a -> s {values = a} :: OperationFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable OperationFilter

instance Prelude.NFData OperationFilter

instance Prelude.ToJSON OperationFilter where
  toJSON OperationFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Condition" Prelude..=) Prelude.<$> condition,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
