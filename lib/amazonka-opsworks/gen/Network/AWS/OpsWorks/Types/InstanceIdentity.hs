{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.InstanceIdentity
  ( InstanceIdentity (..)
  -- * Smart constructor
  , mkInstanceIdentity
  -- * Lenses
  , iiDocument
  , iiSignature
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data> .
--
-- /See:/ 'mkInstanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { document :: Core.Maybe Core.Text
    -- ^ A JSON document that contains the metadata.
  , signature :: Core.Maybe Core.Text
    -- ^ A signature that can be used to verify the document's accuracy and authenticity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceIdentity' value with any optional fields omitted.
mkInstanceIdentity
    :: InstanceIdentity
mkInstanceIdentity
  = InstanceIdentity'{document = Core.Nothing,
                      signature = Core.Nothing}

-- | A JSON document that contains the metadata.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDocument :: Lens.Lens' InstanceIdentity (Core.Maybe Core.Text)
iiDocument = Lens.field @"document"
{-# INLINEABLE iiDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | A signature that can be used to verify the document's accuracy and authenticity.
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
                 [("Document" Core..=) Core.<$> document,
                  ("Signature" Core..=) Core.<$> signature])
