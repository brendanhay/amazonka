{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParameterHistory
  ( ParameterHistory (..)
  -- * Smart constructor
  , mkParameterHistory
  -- * Lenses
  , phAllowedPattern
  , phDataType
  , phDescription
  , phKeyId
  , phLabels
  , phLastModifiedDate
  , phLastModifiedUser
  , phName
  , phPolicies
  , phTier
  , phType
  , phValue
  , phVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AllowedPattern as Types
import qualified Network.AWS.SSM.Types.DataType as Types
import qualified Network.AWS.SSM.Types.PSParameterName as Types
import qualified Network.AWS.SSM.Types.ParameterDescription as Types
import qualified Network.AWS.SSM.Types.ParameterInlinePolicy as Types
import qualified Network.AWS.SSM.Types.ParameterKeyId as Types
import qualified Network.AWS.SSM.Types.ParameterLabel as Types
import qualified Network.AWS.SSM.Types.ParameterTier as Types
import qualified Network.AWS.SSM.Types.ParameterType as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | Information about parameter usage.
--
-- /See:/ 'mkParameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { allowedPattern :: Core.Maybe Types.AllowedPattern
    -- ^ Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
  , dataType :: Core.Maybe Types.DataType
    -- ^ The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
  , description :: Core.Maybe Types.ParameterDescription
    -- ^ Information about the parameter.
  , keyId :: Core.Maybe Types.ParameterKeyId
    -- ^ The ID of the query key used for this parameter.
  , labels :: Core.Maybe (Core.NonEmpty Types.ParameterLabel)
    -- ^ Labels assigned to the parameter version.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date the parameter was last changed or updated.
  , lastModifiedUser :: Core.Maybe Core.Text
    -- ^ Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
  , name :: Core.Maybe Types.PSParameterName
    -- ^ The name of the parameter.
  , policies :: Core.Maybe [Types.ParameterInlinePolicy]
    -- ^ Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
  , tier :: Core.Maybe Types.ParameterTier
    -- ^ The parameter tier.
  , type' :: Core.Maybe Types.ParameterType
    -- ^ The type of parameter used.
  , value :: Core.Maybe Types.Value
    -- ^ The parameter value.
  , version :: Core.Maybe Core.Integer
    -- ^ The parameter version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ParameterHistory' value with any optional fields omitted.
mkParameterHistory
    :: ParameterHistory
mkParameterHistory
  = ParameterHistory'{allowedPattern = Core.Nothing,
                      dataType = Core.Nothing, description = Core.Nothing,
                      keyId = Core.Nothing, labels = Core.Nothing,
                      lastModifiedDate = Core.Nothing, lastModifiedUser = Core.Nothing,
                      name = Core.Nothing, policies = Core.Nothing, tier = Core.Nothing,
                      type' = Core.Nothing, value = Core.Nothing, version = Core.Nothing}

-- | Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phAllowedPattern :: Lens.Lens' ParameterHistory (Core.Maybe Types.AllowedPattern)
phAllowedPattern = Lens.field @"allowedPattern"
{-# INLINEABLE phAllowedPattern #-}
{-# DEPRECATED allowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead"  #-}

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phDataType :: Lens.Lens' ParameterHistory (Core.Maybe Types.DataType)
phDataType = Lens.field @"dataType"
{-# INLINEABLE phDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | Information about the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phDescription :: Lens.Lens' ParameterHistory (Core.Maybe Types.ParameterDescription)
phDescription = Lens.field @"description"
{-# INLINEABLE phDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the query key used for this parameter.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phKeyId :: Lens.Lens' ParameterHistory (Core.Maybe Types.ParameterKeyId)
phKeyId = Lens.field @"keyId"
{-# INLINEABLE phKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Labels assigned to the parameter version.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLabels :: Lens.Lens' ParameterHistory (Core.Maybe (Core.NonEmpty Types.ParameterLabel))
phLabels = Lens.field @"labels"
{-# INLINEABLE phLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | Date the parameter was last changed or updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLastModifiedDate :: Lens.Lens' ParameterHistory (Core.Maybe Core.NominalDiffTime)
phLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE phLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phLastModifiedUser :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
phLastModifiedUser = Lens.field @"lastModifiedUser"
{-# INLINEABLE phLastModifiedUser #-}
{-# DEPRECATED lastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead"  #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phName :: Lens.Lens' ParameterHistory (Core.Maybe Types.PSParameterName)
phName = Lens.field @"name"
{-# INLINEABLE phName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phPolicies :: Lens.Lens' ParameterHistory (Core.Maybe [Types.ParameterInlinePolicy])
phPolicies = Lens.field @"policies"
{-# INLINEABLE phPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The parameter tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phTier :: Lens.Lens' ParameterHistory (Core.Maybe Types.ParameterTier)
phTier = Lens.field @"tier"
{-# INLINEABLE phTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The type of parameter used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phType :: Lens.Lens' ParameterHistory (Core.Maybe Types.ParameterType)
phType = Lens.field @"type'"
{-# INLINEABLE phType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phValue :: Lens.Lens' ParameterHistory (Core.Maybe Types.Value)
phValue = Lens.field @"value"
{-# INLINEABLE phValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The parameter version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phVersion :: Lens.Lens' ParameterHistory (Core.Maybe Core.Integer)
phVersion = Lens.field @"version"
{-# INLINEABLE phVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ParameterHistory where
        parseJSON
          = Core.withObject "ParameterHistory" Core.$
              \ x ->
                ParameterHistory' Core.<$>
                  (x Core..:? "AllowedPattern") Core.<*> x Core..:? "DataType"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "KeyId"
                    Core.<*> x Core..:? "Labels"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "LastModifiedUser"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Policies"
                    Core.<*> x Core..:? "Tier"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "Value"
                    Core.<*> x Core..:? "Version"
