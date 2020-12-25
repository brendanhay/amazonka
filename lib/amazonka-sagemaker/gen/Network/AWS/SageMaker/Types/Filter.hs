{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fOperator,
    fValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Operator as Types
import qualified Network.AWS.SageMaker.Types.ResourcePropertyName as Types
import qualified Network.AWS.SageMaker.Types.Value as Types

-- | A conditional statement for a search expression that includes a resource property, a Boolean operator, and a value. Resources that match the statement are returned in the results from the 'Search' API.
--
-- If you specify a @Value@ , but not an @Operator@ , Amazon SageMaker uses the equals operator.
-- In search, there are several property types:
--
--     * Metrics
--
--     * To define a metric filter, enter a value using the form @"Metrics.<name>"@ , where @<name>@ is a metric name. For example, the following filter searches for training jobs with an @"accuracy"@ metric greater than @"0.9"@ :
-- @{@
-- @"Name": "Metrics.accuracy",@
-- @"Operator": "GreaterThan",@
-- @"Value": "0.9"@
-- @}@
--
--
--     * HyperParameters
--
--     * To define a hyperparameter filter, enter a value with the form @"HyperParameters.<name>"@ . Decimal hyperparameter values are treated as a decimal in a comparison if the specified @Value@ is also a decimal value. If the specified @Value@ is an integer, the decimal hyperparameter values are treated as integers. For example, the following filter is satisfied by training jobs with a @"learning_rate"@ hyperparameter that is less than @"0.5"@ :
-- @{@
-- @"Name": "HyperParameters.learning_rate",@
-- @"Operator": "LessThan",@
-- @"Value": "0.5"@
-- @}@
--
--
--     * Tags
--
--     * To define a tag filter, enter a value with the form @Tags.<key>@ .
--
--
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | A resource property name. For example, @TrainingJobName@ . For valid property names, see 'SearchRecord' . You must specify a valid property for the resource.
    name :: Types.ResourcePropertyName,
    -- | A Boolean binary operator that is used to evaluate the filter. The operator field contains one of the following values:
    --
    --
    --     * Equals
    --
    --     * The value of @Name@ equals @Value@ .
    --
    --
    --     * NotEquals
    --
    --     * The value of @Name@ doesn't equal @Value@ .
    --
    --
    --     * Exists
    --
    --     * The @Name@ property exists.
    --
    --
    --     * NotExists
    --
    --     * The @Name@ property does not exist.
    --
    --
    --     * GreaterThan
    --
    --     * The value of @Name@ is greater than @Value@ . Not supported for text properties.
    --
    --
    --     * GreaterThanOrEqualTo
    --
    --     * The value of @Name@ is greater than or equal to @Value@ . Not supported for text properties.
    --
    --
    --     * LessThan
    --
    --     * The value of @Name@ is less than @Value@ . Not supported for text properties.
    --
    --
    --     * LessThanOrEqualTo
    --
    --     * The value of @Name@ is less than or equal to @Value@ . Not supported for text properties.
    --
    --
    --     * In
    --
    --     * The value of @Name@ is one of the comma delimited strings in @Value@ . Only supported for text properties.
    --
    --
    --     * Contains
    --
    --     * The value of @Name@ contains the string @Value@ . Only supported for text properties.
    -- A @SearchExpression@ can include the @Contains@ operator multiple times when the value of @Name@ is one of the following:
    --
    --     * @Experiment.DisplayName@
    --
    --
    --     * @Experiment.ExperimentName@
    --
    --
    --     * @Experiment.Tags@
    --
    --
    --     * @Trial.DisplayName@
    --
    --
    --     * @Trial.TrialName@
    --
    --
    --     * @Trial.Tags@
    --
    --
    --     * @TrialComponent.DisplayName@
    --
    --
    --     * @TrialComponent.TrialComponentName@
    --
    --
    --     * @TrialComponent.Tags@
    --
    --
    --     * @TrialComponent.InputArtifacts@
    --
    --
    --     * @TrialComponent.OutputArtifacts@
    --
    --
    -- A @SearchExpression@ can include only one @Contains@ operator for all other values of @Name@ . In these cases, if you include multiple @Contains@ operators in the @SearchExpression@ , the result is the following error message: "@'CONTAINS' operator usage limit of 1 exceeded.@ "
    operator :: Core.Maybe Types.Operator,
    -- | A value used with @Name@ and @Operator@ to determine which resources satisfy the filter's condition. For numerical properties, @Value@ must be an integer or floating-point decimal. For timestamp properties, @Value@ must be an ISO 8601 date-time string of the following format: @YYYY-mm-dd'T'HH:MM:SS@ .
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  -- | 'name'
  Types.ResourcePropertyName ->
  Filter
mkFilter name =
  Filter' {name, operator = Core.Nothing, value = Core.Nothing}

-- | A resource property name. For example, @TrainingJobName@ . For valid property names, see 'SearchRecord' . You must specify a valid property for the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Types.ResourcePropertyName
fName = Lens.field @"name"
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean binary operator that is used to evaluate the filter. The operator field contains one of the following values:
--
--
--     * Equals
--
--     * The value of @Name@ equals @Value@ .
--
--
--     * NotEquals
--
--     * The value of @Name@ doesn't equal @Value@ .
--
--
--     * Exists
--
--     * The @Name@ property exists.
--
--
--     * NotExists
--
--     * The @Name@ property does not exist.
--
--
--     * GreaterThan
--
--     * The value of @Name@ is greater than @Value@ . Not supported for text properties.
--
--
--     * GreaterThanOrEqualTo
--
--     * The value of @Name@ is greater than or equal to @Value@ . Not supported for text properties.
--
--
--     * LessThan
--
--     * The value of @Name@ is less than @Value@ . Not supported for text properties.
--
--
--     * LessThanOrEqualTo
--
--     * The value of @Name@ is less than or equal to @Value@ . Not supported for text properties.
--
--
--     * In
--
--     * The value of @Name@ is one of the comma delimited strings in @Value@ . Only supported for text properties.
--
--
--     * Contains
--
--     * The value of @Name@ contains the string @Value@ . Only supported for text properties.
-- A @SearchExpression@ can include the @Contains@ operator multiple times when the value of @Name@ is one of the following:
--
--     * @Experiment.DisplayName@
--
--
--     * @Experiment.ExperimentName@
--
--
--     * @Experiment.Tags@
--
--
--     * @Trial.DisplayName@
--
--
--     * @Trial.TrialName@
--
--
--     * @Trial.Tags@
--
--
--     * @TrialComponent.DisplayName@
--
--
--     * @TrialComponent.TrialComponentName@
--
--
--     * @TrialComponent.Tags@
--
--
--     * @TrialComponent.InputArtifacts@
--
--
--     * @TrialComponent.OutputArtifacts@
--
--
-- A @SearchExpression@ can include only one @Contains@ operator for all other values of @Name@ . In these cases, if you include multiple @Contains@ operators in the @SearchExpression@ , the result is the following error message: "@'CONTAINS' operator usage limit of 1 exceeded.@ "
--
--
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fOperator :: Lens.Lens' Filter (Core.Maybe Types.Operator)
fOperator = Lens.field @"operator"
{-# DEPRECATED fOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | A value used with @Name@ and @Operator@ to determine which resources satisfy the filter's condition. For numerical properties, @Value@ must be an integer or floating-point decimal. For timestamp properties, @Value@ must be an ISO 8601 date-time string of the following format: @YYYY-mm-dd'T'HH:MM:SS@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValue :: Lens.Lens' Filter (Core.Maybe Types.Value)
fValue = Lens.field @"value"
{-# DEPRECATED fValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Filter where
  toJSON Filter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Operator" Core..=) Core.<$> operator,
            ("Value" Core..=) Core.<$> value
          ]
      )
