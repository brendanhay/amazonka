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
    fFunctionARN,
    fFunctionConfiguration,
    fId,
  )
where

import Network.AWS.Greengrass.Types.FunctionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Lambda function.
--
-- /See:/ 'mkFunction' smart constructor.
data Function = Function'
  { functionARN :: Lude.Maybe Lude.Text,
    functionConfiguration :: Lude.Maybe FunctionConfiguration,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Function' with the minimum fields required to make a request.
--
-- * 'functionARN' - The ARN of the Lambda function.
-- * 'functionConfiguration' - The configuration of the Lambda function.
-- * 'id' - A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
mkFunction ::
  -- | 'id'
  Lude.Text ->
  Function
mkFunction pId_ =
  Function'
    { functionARN = Lude.Nothing,
      functionConfiguration = Lude.Nothing,
      id = pId_
    }

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFunctionARN :: Lens.Lens' Function (Lude.Maybe Lude.Text)
fFunctionARN = Lens.lens (functionARN :: Function -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: Function)
{-# DEPRECATED fFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | The configuration of the Lambda function.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFunctionConfiguration :: Lens.Lens' Function (Lude.Maybe FunctionConfiguration)
fFunctionConfiguration = Lens.lens (functionConfiguration :: Function -> Lude.Maybe FunctionConfiguration) (\s a -> s {functionConfiguration = a} :: Function)
{-# DEPRECATED fFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Function Lude.Text
fId = Lens.lens (id :: Function -> Lude.Text) (\s a -> s {id = a} :: Function)
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Function where
  parseJSON =
    Lude.withObject
      "Function"
      ( \x ->
          Function'
            Lude.<$> (x Lude..:? "FunctionArn")
            Lude.<*> (x Lude..:? "FunctionConfiguration")
            Lude.<*> (x Lude..: "Id")
      )

instance Lude.ToJSON Function where
  toJSON Function' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FunctionArn" Lude..=) Lude.<$> functionARN,
            ("FunctionConfiguration" Lude..=) Lude.<$> functionConfiguration,
            Lude.Just ("Id" Lude..= id)
          ]
      )
