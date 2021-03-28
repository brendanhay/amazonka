{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PublicKeySummary
  ( PublicKeySummary (..)
  -- * Smart constructor
  , mkPublicKeySummary
  -- * Lenses
  , pksId
  , pksName
  , pksCreatedTime
  , pksEncodedKey
  , pksComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a public key.
--
-- /See:/ 'mkPublicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { id :: Core.Text
    -- ^ The identifier of the public key.
  , name :: Core.Text
    -- ^ A name to help identify the public key.
  , createdTime :: Core.UTCTime
    -- ^ The date and time when the public key was uploaded.
  , encodedKey :: Core.Text
    -- ^ The public key.
  , comment :: Core.Maybe Core.Text
    -- ^ A comment to describe the public key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PublicKeySummary' value with any optional fields omitted.
mkPublicKeySummary
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'name'
    -> Core.UTCTime -- ^ 'createdTime'
    -> Core.Text -- ^ 'encodedKey'
    -> PublicKeySummary
mkPublicKeySummary id name createdTime encodedKey
  = PublicKeySummary'{id, name, createdTime, encodedKey,
                      comment = Core.Nothing}

-- | The identifier of the public key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksId :: Lens.Lens' PublicKeySummary Core.Text
pksId = Lens.field @"id"
{-# INLINEABLE pksId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksName :: Lens.Lens' PublicKeySummary Core.Text
pksName = Lens.field @"name"
{-# INLINEABLE pksName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The date and time when the public key was uploaded.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksCreatedTime :: Lens.Lens' PublicKeySummary Core.UTCTime
pksCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE pksCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The public key.
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksEncodedKey :: Lens.Lens' PublicKeySummary Core.Text
pksEncodedKey = Lens.field @"encodedKey"
{-# INLINEABLE pksEncodedKey #-}
{-# DEPRECATED encodedKey "Use generic-lens or generic-optics with 'encodedKey' instead"  #-}

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pksComment :: Lens.Lens' PublicKeySummary (Core.Maybe Core.Text)
pksComment = Lens.field @"comment"
{-# INLINEABLE pksComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.FromXML PublicKeySummary where
        parseXML x
          = PublicKeySummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "Name" Core.<*>
                x Core..@ "CreatedTime"
                Core.<*> x Core..@ "EncodedKey"
                Core.<*> x Core..@? "Comment"
