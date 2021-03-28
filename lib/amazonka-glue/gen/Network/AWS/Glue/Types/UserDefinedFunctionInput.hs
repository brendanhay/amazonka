{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunctionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.UserDefinedFunctionInput
  ( UserDefinedFunctionInput (..)
  -- * Smart constructor
  , mkUserDefinedFunctionInput
  -- * Lenses
  , udfiClassName
  , udfiFunctionName
  , udfiOwnerName
  , udfiOwnerType
  , udfiResourceUris
  ) where

import qualified Network.AWS.Glue.Types.ClassName as Types
import qualified Network.AWS.Glue.Types.FunctionName as Types
import qualified Network.AWS.Glue.Types.OwnerName as Types
import qualified Network.AWS.Glue.Types.PrincipalType as Types
import qualified Network.AWS.Glue.Types.ResourceUri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure used to create or update a user-defined function.
--
-- /See:/ 'mkUserDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { className :: Core.Maybe Types.ClassName
    -- ^ The Java class that contains the function code.
  , functionName :: Core.Maybe Types.FunctionName
    -- ^ The name of the function.
  , ownerName :: Core.Maybe Types.OwnerName
    -- ^ The owner of the function.
  , ownerType :: Core.Maybe Types.PrincipalType
    -- ^ The owner type.
  , resourceUris :: Core.Maybe [Types.ResourceUri]
    -- ^ The resource URIs for the function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserDefinedFunctionInput' value with any optional fields omitted.
mkUserDefinedFunctionInput
    :: UserDefinedFunctionInput
mkUserDefinedFunctionInput
  = UserDefinedFunctionInput'{className = Core.Nothing,
                              functionName = Core.Nothing, ownerName = Core.Nothing,
                              ownerType = Core.Nothing, resourceUris = Core.Nothing}

-- | The Java class that contains the function code.
--
-- /Note:/ Consider using 'className' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiClassName :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Types.ClassName)
udfiClassName = Lens.field @"className"
{-# INLINEABLE udfiClassName #-}
{-# DEPRECATED className "Use generic-lens or generic-optics with 'className' instead"  #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiFunctionName :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Types.FunctionName)
udfiFunctionName = Lens.field @"functionName"
{-# INLINEABLE udfiFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The owner of the function.
--
-- /Note:/ Consider using 'ownerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiOwnerName :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Types.OwnerName)
udfiOwnerName = Lens.field @"ownerName"
{-# INLINEABLE udfiOwnerName #-}
{-# DEPRECATED ownerName "Use generic-lens or generic-optics with 'ownerName' instead"  #-}

-- | The owner type.
--
-- /Note:/ Consider using 'ownerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiOwnerType :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Types.PrincipalType)
udfiOwnerType = Lens.field @"ownerType"
{-# INLINEABLE udfiOwnerType #-}
{-# DEPRECATED ownerType "Use generic-lens or generic-optics with 'ownerType' instead"  #-}

-- | The resource URIs for the function.
--
-- /Note:/ Consider using 'resourceUris' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiResourceUris :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe [Types.ResourceUri])
udfiResourceUris = Lens.field @"resourceUris"
{-# INLINEABLE udfiResourceUris #-}
{-# DEPRECATED resourceUris "Use generic-lens or generic-optics with 'resourceUris' instead"  #-}

instance Core.FromJSON UserDefinedFunctionInput where
        toJSON UserDefinedFunctionInput{..}
          = Core.object
              (Core.catMaybes
                 [("ClassName" Core..=) Core.<$> className,
                  ("FunctionName" Core..=) Core.<$> functionName,
                  ("OwnerName" Core..=) Core.<$> ownerName,
                  ("OwnerType" Core..=) Core.<$> ownerType,
                  ("ResourceUris" Core..=) Core.<$> resourceUris])
