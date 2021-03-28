{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.TypeSummary
  ( TypeSummary (..)
  -- * Smart constructor
  , mkTypeSummary
  -- * Lenses
  , tsDefaultVersionId
  , tsDescription
  , tsLastUpdated
  , tsType
  , tsTypeArn
  , tsTypeName
  ) where

import qualified Network.AWS.CloudFormation.Types.DefaultVersionId as Types
import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.RegistryType as Types
import qualified Network.AWS.CloudFormation.Types.TypeArn as Types
import qualified Network.AWS.CloudFormation.Types.TypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about the specified CloudFormation type.
--
-- /See:/ 'mkTypeSummary' smart constructor.
data TypeSummary = TypeSummary'
  { defaultVersionId :: Core.Maybe Types.DefaultVersionId
    -- ^ The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ . 
  , description :: Core.Maybe Types.Description
    -- ^ The description of the type.
  , lastUpdated :: Core.Maybe Core.UTCTime
    -- ^ When the current default version of the type was registered.
  , type' :: Core.Maybe Types.RegistryType
    -- ^ The kind of type.
  , typeArn :: Core.Maybe Types.TypeArn
    -- ^ The Amazon Resource Name (ARN) of the type.
  , typeName :: Core.Maybe Types.TypeName
    -- ^ The name of the type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypeSummary' value with any optional fields omitted.
mkTypeSummary
    :: TypeSummary
mkTypeSummary
  = TypeSummary'{defaultVersionId = Core.Nothing,
                 description = Core.Nothing, lastUpdated = Core.Nothing,
                 type' = Core.Nothing, typeArn = Core.Nothing,
                 typeName = Core.Nothing}

-- | The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ . 
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDefaultVersionId :: Lens.Lens' TypeSummary (Core.Maybe Types.DefaultVersionId)
tsDefaultVersionId = Lens.field @"defaultVersionId"
{-# INLINEABLE tsDefaultVersionId #-}
{-# DEPRECATED defaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead"  #-}

-- | The description of the type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDescription :: Lens.Lens' TypeSummary (Core.Maybe Types.Description)
tsDescription = Lens.field @"description"
{-# INLINEABLE tsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | When the current default version of the type was registered.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastUpdated :: Lens.Lens' TypeSummary (Core.Maybe Core.UTCTime)
tsLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE tsLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | The kind of type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsType :: Lens.Lens' TypeSummary (Core.Maybe Types.RegistryType)
tsType = Lens.field @"type'"
{-# INLINEABLE tsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- /Note:/ Consider using 'typeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTypeArn :: Lens.Lens' TypeSummary (Core.Maybe Types.TypeArn)
tsTypeArn = Lens.field @"typeArn"
{-# INLINEABLE tsTypeArn #-}
{-# DEPRECATED typeArn "Use generic-lens or generic-optics with 'typeArn' instead"  #-}

-- | The name of the type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTypeName :: Lens.Lens' TypeSummary (Core.Maybe Types.TypeName)
tsTypeName = Lens.field @"typeName"
{-# INLINEABLE tsTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.FromXML TypeSummary where
        parseXML x
          = TypeSummary' Core.<$>
              (x Core..@? "DefaultVersionId") Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "LastUpdated"
                Core.<*> x Core..@? "Type"
                Core.<*> x Core..@? "TypeArn"
                Core.<*> x Core..@? "TypeName"
