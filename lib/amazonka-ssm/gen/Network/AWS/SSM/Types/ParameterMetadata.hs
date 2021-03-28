{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParameterMetadata
  ( ParameterMetadata (..)
  -- * Smart constructor
  , mkParameterMetadata
  -- * Lenses
  , pmAllowedPattern
  , pmDataType
  , pmDescription
  , pmKeyId
  , pmLastModifiedDate
  , pmLastModifiedUser
  , pmName
  , pmPolicies
  , pmTier
  , pmType
  , pmVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AllowedPattern as Types
import qualified Network.AWS.SSM.Types.DataType as Types
import qualified Network.AWS.SSM.Types.Description as Types
import qualified Network.AWS.SSM.Types.KeyId as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.ParameterInlinePolicy as Types
import qualified Network.AWS.SSM.Types.ParameterTier as Types
import qualified Network.AWS.SSM.Types.ParameterType as Types

-- | Metadata includes information like the ARN of the last user and the date/time the parameter was last used.
--
-- /See:/ 'mkParameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
  { allowedPattern :: Core.Maybe Types.AllowedPattern
    -- ^ A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
  , dataType :: Core.Maybe Types.DataType
    -- ^ The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
  , description :: Core.Maybe Types.Description
    -- ^ Description of the parameter actions.
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The ID of the query key used for this parameter.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date the parameter was last changed or updated.
  , lastModifiedUser :: Core.Maybe Core.Text
    -- ^ Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
  , name :: Core.Maybe Types.Name
    -- ^ The parameter name.
  , policies :: Core.Maybe [Types.ParameterInlinePolicy]
    -- ^ A list of policies associated with a parameter.
  , tier :: Core.Maybe Types.ParameterTier
    -- ^ The parameter tier.
  , type' :: Core.Maybe Types.ParameterType
    -- ^ The type of parameter. Valid parameter types include the following: @String@ , @StringList@ , and @SecureString@ .
  , version :: Core.Maybe Core.Integer
    -- ^ The parameter version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ParameterMetadata' value with any optional fields omitted.
mkParameterMetadata
    :: ParameterMetadata
mkParameterMetadata
  = ParameterMetadata'{allowedPattern = Core.Nothing,
                       dataType = Core.Nothing, description = Core.Nothing,
                       keyId = Core.Nothing, lastModifiedDate = Core.Nothing,
                       lastModifiedUser = Core.Nothing, name = Core.Nothing,
                       policies = Core.Nothing, tier = Core.Nothing, type' = Core.Nothing,
                       version = Core.Nothing}

-- | A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmAllowedPattern :: Lens.Lens' ParameterMetadata (Core.Maybe Types.AllowedPattern)
pmAllowedPattern = Lens.field @"allowedPattern"
{-# INLINEABLE pmAllowedPattern #-}
{-# DEPRECATED allowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead"  #-}

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmDataType :: Lens.Lens' ParameterMetadata (Core.Maybe Types.DataType)
pmDataType = Lens.field @"dataType"
{-# INLINEABLE pmDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | Description of the parameter actions.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmDescription :: Lens.Lens' ParameterMetadata (Core.Maybe Types.Description)
pmDescription = Lens.field @"description"
{-# INLINEABLE pmDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the query key used for this parameter.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmKeyId :: Lens.Lens' ParameterMetadata (Core.Maybe Types.KeyId)
pmKeyId = Lens.field @"keyId"
{-# INLINEABLE pmKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Date the parameter was last changed or updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmLastModifiedDate :: Lens.Lens' ParameterMetadata (Core.Maybe Core.NominalDiffTime)
pmLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE pmLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmLastModifiedUser :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
pmLastModifiedUser = Lens.field @"lastModifiedUser"
{-# INLINEABLE pmLastModifiedUser #-}
{-# DEPRECATED lastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead"  #-}

-- | The parameter name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmName :: Lens.Lens' ParameterMetadata (Core.Maybe Types.Name)
pmName = Lens.field @"name"
{-# INLINEABLE pmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of policies associated with a parameter.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPolicies :: Lens.Lens' ParameterMetadata (Core.Maybe [Types.ParameterInlinePolicy])
pmPolicies = Lens.field @"policies"
{-# INLINEABLE pmPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The parameter tier.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTier :: Lens.Lens' ParameterMetadata (Core.Maybe Types.ParameterTier)
pmTier = Lens.field @"tier"
{-# INLINEABLE pmTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The type of parameter. Valid parameter types include the following: @String@ , @StringList@ , and @SecureString@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmType :: Lens.Lens' ParameterMetadata (Core.Maybe Types.ParameterType)
pmType = Lens.field @"type'"
{-# INLINEABLE pmType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The parameter version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmVersion :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Integer)
pmVersion = Lens.field @"version"
{-# INLINEABLE pmVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ParameterMetadata where
        parseJSON
          = Core.withObject "ParameterMetadata" Core.$
              \ x ->
                ParameterMetadata' Core.<$>
                  (x Core..:? "AllowedPattern") Core.<*> x Core..:? "DataType"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "KeyId"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "LastModifiedUser"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Policies"
                    Core.<*> x Core..:? "Tier"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "Version"
