{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Function
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Function
  ( Function (..),

    -- * Smart constructor
    mkFunction,

    -- * Lenses
    fId,
    fFunctionArn,
    fFunctionConfiguration,
  )
where

import qualified Network.AWS.Greengrass.Types.FunctionConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Lambda function.
--
-- /See:/ 'mkFunction' smart constructor.
data Function = Function'
  { -- | A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Core.Text,
    -- | The ARN of the Lambda function.
    functionArn :: Core.Maybe Core.Text,
    -- | The configuration of the Lambda function.
    functionConfiguration :: Core.Maybe Types.FunctionConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Function' value with any optional fields omitted.
mkFunction ::
  -- | 'id'
  Core.Text ->
  Function
mkFunction id =
  Function'
    { id,
      functionArn = Core.Nothing,
      functionConfiguration = Core.Nothing
    }

-- | A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Function Core.Text
fId = Lens.field @"id"
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFunctionArn :: Lens.Lens' Function (Core.Maybe Core.Text)
fFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED fFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | The configuration of the Lambda function.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFunctionConfiguration :: Lens.Lens' Function (Core.Maybe Types.FunctionConfiguration)
fFunctionConfiguration = Lens.field @"functionConfiguration"
{-# DEPRECATED fFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

instance Core.FromJSON Function where
  toJSON Function {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("FunctionArn" Core..=) Core.<$> functionArn,
            ("FunctionConfiguration" Core..=) Core.<$> functionConfiguration
          ]
      )

instance Core.FromJSON Function where
  parseJSON =
    Core.withObject "Function" Core.$
      \x ->
        Function'
          Core.<$> (x Core..: "Id")
          Core.<*> (x Core..:? "FunctionArn")
          Core.<*> (x Core..:? "FunctionConfiguration")
