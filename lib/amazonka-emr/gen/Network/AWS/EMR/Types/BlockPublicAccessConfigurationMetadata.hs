{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
  ( BlockPublicAccessConfigurationMetadata (..)
  -- * Smart constructor
  , mkBlockPublicAccessConfigurationMetadata
  -- * Lenses
  , bpacmCreationDateTime
  , bpacmCreatedByArn
  ) where

import qualified Network.AWS.EMR.Types.ArnType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
--
-- /See:/ 'mkBlockPublicAccessConfigurationMetadata' smart constructor.
data BlockPublicAccessConfigurationMetadata = BlockPublicAccessConfigurationMetadata'
  { creationDateTime :: Core.NominalDiffTime
    -- ^ The date and time that the configuration was created.
  , createdByArn :: Types.ArnType
    -- ^ The Amazon Resource Name that created or last modified the configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BlockPublicAccessConfigurationMetadata' value with any optional fields omitted.
mkBlockPublicAccessConfigurationMetadata
    :: Core.NominalDiffTime -- ^ 'creationDateTime'
    -> Types.ArnType -- ^ 'createdByArn'
    -> BlockPublicAccessConfigurationMetadata
mkBlockPublicAccessConfigurationMetadata creationDateTime
  createdByArn
  = BlockPublicAccessConfigurationMetadata'{creationDateTime,
                                            createdByArn}

-- | The date and time that the configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacmCreationDateTime :: Lens.Lens' BlockPublicAccessConfigurationMetadata Core.NominalDiffTime
bpacmCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE bpacmCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The Amazon Resource Name that created or last modified the configuration.
--
-- /Note:/ Consider using 'createdByArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacmCreatedByArn :: Lens.Lens' BlockPublicAccessConfigurationMetadata Types.ArnType
bpacmCreatedByArn = Lens.field @"createdByArn"
{-# INLINEABLE bpacmCreatedByArn #-}
{-# DEPRECATED createdByArn "Use generic-lens or generic-optics with 'createdByArn' instead"  #-}

instance Core.FromJSON BlockPublicAccessConfigurationMetadata where
        parseJSON
          = Core.withObject "BlockPublicAccessConfigurationMetadata" Core.$
              \ x ->
                BlockPublicAccessConfigurationMetadata' Core.<$>
                  (x Core..: "CreationDateTime") Core.<*> x Core..: "CreatedByArn"
