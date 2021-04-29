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
-- Module      : Network.AWS.SageMaker.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Filter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.Operator

-- | A conditional statement for a search expression that includes a resource
-- property, a Boolean operator, and a value. Resources that match the
-- statement are returned in the results from the Search API.
--
-- If you specify a @Value@, but not an @Operator@, Amazon SageMaker uses
-- the equals operator.
--
-- In search, there are several property types:
--
-- [Metrics]
--     To define a metric filter, enter a value using the form
--     @\"Metrics.\<name>\"@, where @\<name>@ is a metric name. For
--     example, the following filter searches for training jobs with an
--     @\"accuracy\"@ metric greater than @\"0.9\"@:
--
--     @{@
--
--     @\"Name\": \"Metrics.accuracy\",@
--
--     @\"Operator\": \"GreaterThan\",@
--
--     @\"Value\": \"0.9\"@
--
--     @}@
--
-- [HyperParameters]
--     To define a hyperparameter filter, enter a value with the form
--     @\"HyperParameters.\<name>\"@. Decimal hyperparameter values are
--     treated as a decimal in a comparison if the specified @Value@ is
--     also a decimal value. If the specified @Value@ is an integer, the
--     decimal hyperparameter values are treated as integers. For example,
--     the following filter is satisfied by training jobs with a
--     @\"learning_rate\"@ hyperparameter that is less than @\"0.5\"@:
--
--     @ {@
--
--     @ \"Name\": \"HyperParameters.learning_rate\",@
--
--     @ \"Operator\": \"LessThan\",@
--
--     @ \"Value\": \"0.5\"@
--
--     @ }@
--
-- [Tags]
--     To define a tag filter, enter a value with the form @Tags.\<key>@.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | A Boolean binary operator that is used to evaluate the filter. The
    -- operator field contains one of the following values:
    --
    -- [Equals]
    --     The value of @Name@ equals @Value@.
    --
    -- [NotEquals]
    --     The value of @Name@ doesn\'t equal @Value@.
    --
    -- [Exists]
    --     The @Name@ property exists.
    --
    -- [NotExists]
    --     The @Name@ property does not exist.
    --
    -- [GreaterThan]
    --     The value of @Name@ is greater than @Value@. Not supported for text
    --     properties.
    --
    -- [GreaterThanOrEqualTo]
    --     The value of @Name@ is greater than or equal to @Value@. Not
    --     supported for text properties.
    --
    -- [LessThan]
    --     The value of @Name@ is less than @Value@. Not supported for text
    --     properties.
    --
    -- [LessThanOrEqualTo]
    --     The value of @Name@ is less than or equal to @Value@. Not supported
    --     for text properties.
    --
    -- [In]
    --     The value of @Name@ is one of the comma delimited strings in
    --     @Value@. Only supported for text properties.
    --
    -- [Contains]
    --     The value of @Name@ contains the string @Value@. Only supported for
    --     text properties.
    --
    --     A @SearchExpression@ can include the @Contains@ operator multiple
    --     times when the value of @Name@ is one of the following:
    --
    --     -   @Experiment.DisplayName@
    --
    --     -   @Experiment.ExperimentName@
    --
    --     -   @Experiment.Tags@
    --
    --     -   @Trial.DisplayName@
    --
    --     -   @Trial.TrialName@
    --
    --     -   @Trial.Tags@
    --
    --     -   @TrialComponent.DisplayName@
    --
    --     -   @TrialComponent.TrialComponentName@
    --
    --     -   @TrialComponent.Tags@
    --
    --     -   @TrialComponent.InputArtifacts@
    --
    --     -   @TrialComponent.OutputArtifacts@
    --
    --     A @SearchExpression@ can include only one @Contains@ operator for
    --     all other values of @Name@. In these cases, if you include multiple
    --     @Contains@ operators in the @SearchExpression@, the result is the
    --     following error message:
    --     \"@\'CONTAINS\' operator usage limit of 1 exceeded.@\"
    operator :: Prelude.Maybe Operator,
    -- | A value used with @Name@ and @Operator@ to determine which resources
    -- satisfy the filter\'s condition. For numerical properties, @Value@ must
    -- be an integer or floating-point decimal. For timestamp properties,
    -- @Value@ must be an ISO 8601 date-time string of the following format:
    -- @YYYY-mm-dd\'T\'HH:MM:SS@.
    value :: Prelude.Maybe Prelude.Text,
    -- | A resource property name. For example, @TrainingJobName@. For valid
    -- property names, see SearchRecord. You must specify a valid property for
    -- the resource.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'filter_operator' - A Boolean binary operator that is used to evaluate the filter. The
-- operator field contains one of the following values:
--
-- [Equals]
--     The value of @Name@ equals @Value@.
--
-- [NotEquals]
--     The value of @Name@ doesn\'t equal @Value@.
--
-- [Exists]
--     The @Name@ property exists.
--
-- [NotExists]
--     The @Name@ property does not exist.
--
-- [GreaterThan]
--     The value of @Name@ is greater than @Value@. Not supported for text
--     properties.
--
-- [GreaterThanOrEqualTo]
--     The value of @Name@ is greater than or equal to @Value@. Not
--     supported for text properties.
--
-- [LessThan]
--     The value of @Name@ is less than @Value@. Not supported for text
--     properties.
--
-- [LessThanOrEqualTo]
--     The value of @Name@ is less than or equal to @Value@. Not supported
--     for text properties.
--
-- [In]
--     The value of @Name@ is one of the comma delimited strings in
--     @Value@. Only supported for text properties.
--
-- [Contains]
--     The value of @Name@ contains the string @Value@. Only supported for
--     text properties.
--
--     A @SearchExpression@ can include the @Contains@ operator multiple
--     times when the value of @Name@ is one of the following:
--
--     -   @Experiment.DisplayName@
--
--     -   @Experiment.ExperimentName@
--
--     -   @Experiment.Tags@
--
--     -   @Trial.DisplayName@
--
--     -   @Trial.TrialName@
--
--     -   @Trial.Tags@
--
--     -   @TrialComponent.DisplayName@
--
--     -   @TrialComponent.TrialComponentName@
--
--     -   @TrialComponent.Tags@
--
--     -   @TrialComponent.InputArtifacts@
--
--     -   @TrialComponent.OutputArtifacts@
--
--     A @SearchExpression@ can include only one @Contains@ operator for
--     all other values of @Name@. In these cases, if you include multiple
--     @Contains@ operators in the @SearchExpression@, the result is the
--     following error message:
--     \"@\'CONTAINS\' operator usage limit of 1 exceeded.@\"
--
-- 'value', 'filter_value' - A value used with @Name@ and @Operator@ to determine which resources
-- satisfy the filter\'s condition. For numerical properties, @Value@ must
-- be an integer or floating-point decimal. For timestamp properties,
-- @Value@ must be an ISO 8601 date-time string of the following format:
-- @YYYY-mm-dd\'T\'HH:MM:SS@.
--
-- 'name', 'filter_name' - A resource property name. For example, @TrainingJobName@. For valid
-- property names, see SearchRecord. You must specify a valid property for
-- the resource.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter'
    { operator = Prelude.Nothing,
      value = Prelude.Nothing,
      name = pName_
    }

-- | A Boolean binary operator that is used to evaluate the filter. The
-- operator field contains one of the following values:
--
-- [Equals]
--     The value of @Name@ equals @Value@.
--
-- [NotEquals]
--     The value of @Name@ doesn\'t equal @Value@.
--
-- [Exists]
--     The @Name@ property exists.
--
-- [NotExists]
--     The @Name@ property does not exist.
--
-- [GreaterThan]
--     The value of @Name@ is greater than @Value@. Not supported for text
--     properties.
--
-- [GreaterThanOrEqualTo]
--     The value of @Name@ is greater than or equal to @Value@. Not
--     supported for text properties.
--
-- [LessThan]
--     The value of @Name@ is less than @Value@. Not supported for text
--     properties.
--
-- [LessThanOrEqualTo]
--     The value of @Name@ is less than or equal to @Value@. Not supported
--     for text properties.
--
-- [In]
--     The value of @Name@ is one of the comma delimited strings in
--     @Value@. Only supported for text properties.
--
-- [Contains]
--     The value of @Name@ contains the string @Value@. Only supported for
--     text properties.
--
--     A @SearchExpression@ can include the @Contains@ operator multiple
--     times when the value of @Name@ is one of the following:
--
--     -   @Experiment.DisplayName@
--
--     -   @Experiment.ExperimentName@
--
--     -   @Experiment.Tags@
--
--     -   @Trial.DisplayName@
--
--     -   @Trial.TrialName@
--
--     -   @Trial.Tags@
--
--     -   @TrialComponent.DisplayName@
--
--     -   @TrialComponent.TrialComponentName@
--
--     -   @TrialComponent.Tags@
--
--     -   @TrialComponent.InputArtifacts@
--
--     -   @TrialComponent.OutputArtifacts@
--
--     A @SearchExpression@ can include only one @Contains@ operator for
--     all other values of @Name@. In these cases, if you include multiple
--     @Contains@ operators in the @SearchExpression@, the result is the
--     following error message:
--     \"@\'CONTAINS\' operator usage limit of 1 exceeded.@\"
filter_operator :: Lens.Lens' Filter (Prelude.Maybe Operator)
filter_operator = Lens.lens (\Filter' {operator} -> operator) (\s@Filter' {} a -> s {operator = a} :: Filter)

-- | A value used with @Name@ and @Operator@ to determine which resources
-- satisfy the filter\'s condition. For numerical properties, @Value@ must
-- be an integer or floating-point decimal. For timestamp properties,
-- @Value@ must be an ISO 8601 date-time string of the following format:
-- @YYYY-mm-dd\'T\'HH:MM:SS@.
filter_value :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_value = Lens.lens (\Filter' {value} -> value) (\s@Filter' {} a -> s {value = a} :: Filter)

-- | A resource property name. For example, @TrainingJobName@. For valid
-- property names, see SearchRecord. You must specify a valid property for
-- the resource.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Prelude.Hashable Filter

instance Prelude.NFData Filter

instance Prelude.ToJSON Filter where
  toJSON Filter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Operator" Prelude..=) Prelude.<$> operator,
            ("Value" Prelude..=) Prelude.<$> value,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
