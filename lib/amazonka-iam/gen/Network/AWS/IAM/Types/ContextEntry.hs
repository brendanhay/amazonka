{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextEntry
  ( ContextEntry (..),

    -- * Smart constructor
    mkContextEntry,

    -- * Lenses
    ceContextKeyName,
    ceContextKeyType,
    ceContextKeyValues,
  )
where

import qualified Network.AWS.IAM.Types.ContextKeyNameType as Types
import qualified Network.AWS.IAM.Types.ContextKeyTypeEnum as Types
import qualified Network.AWS.IAM.Types.ContextKeyValueType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the @Condition@ elements of the input policies.
--
-- This data type is used as an input parameter to 'SimulateCustomPolicy' and 'SimulatePrincipalPolicy' .
--
-- /See:/ 'mkContextEntry' smart constructor.
data ContextEntry = ContextEntry'
  { -- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
    contextKeyName :: Core.Maybe Types.ContextKeyNameType,
    -- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
    contextKeyType :: Core.Maybe Types.ContextKeyTypeEnum,
    -- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
    contextKeyValues :: Core.Maybe [Types.ContextKeyValueType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContextEntry' value with any optional fields omitted.
mkContextEntry ::
  ContextEntry
mkContextEntry =
  ContextEntry'
    { contextKeyName = Core.Nothing,
      contextKeyType = Core.Nothing,
      contextKeyValues = Core.Nothing
    }

-- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
--
-- /Note:/ Consider using 'contextKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyName :: Lens.Lens' ContextEntry (Core.Maybe Types.ContextKeyNameType)
ceContextKeyName = Lens.field @"contextKeyName"
{-# DEPRECATED ceContextKeyName "Use generic-lens or generic-optics with 'contextKeyName' instead." #-}

-- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
--
-- /Note:/ Consider using 'contextKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyType :: Lens.Lens' ContextEntry (Core.Maybe Types.ContextKeyTypeEnum)
ceContextKeyType = Lens.field @"contextKeyType"
{-# DEPRECATED ceContextKeyType "Use generic-lens or generic-optics with 'contextKeyType' instead." #-}

-- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
--
-- /Note:/ Consider using 'contextKeyValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyValues :: Lens.Lens' ContextEntry (Core.Maybe [Types.ContextKeyValueType])
ceContextKeyValues = Lens.field @"contextKeyValues"
{-# DEPRECATED ceContextKeyValues "Use generic-lens or generic-optics with 'contextKeyValues' instead." #-}
