{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.InstanceIdentity
  ( InstanceIdentity (..)
  -- * Smart constructor
  , mkInstanceIdentity
  -- * Lenses
  , iiDocument
  , iiSignature
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value by calling a metadata URI from the EC2 instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
--
--
-- /See:/ 'mkInstanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { document :: Core.Maybe Core.Text
    -- ^ A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
  , signature :: Core.Maybe Core.Text
    -- ^ A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceIdentity' value with any optional fields omitted.
mkInstanceIdentity
    :: InstanceIdentity
mkInstanceIdentity
  = InstanceIdentity'{document = Core.Nothing,
                      signature = Core.Nothing}

-- | A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDocument :: Lens.Lens' InstanceIdentity (Core.Maybe Core.Text)
iiDocument = Lens.field @"document"
{-# INLINEABLE iiDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiSignature :: Lens.Lens' InstanceIdentity (Core.Maybe Core.Text)
iiSignature = Lens.field @"signature"
{-# INLINEABLE iiSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

instance Core.FromJSON InstanceIdentity where
        toJSON InstanceIdentity{..}
          = Core.object
              (Core.catMaybes
                 [("document" Core..=) Core.<$> document,
                  ("signature" Core..=) Core.<$> signature])
