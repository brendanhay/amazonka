{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AliasListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.AliasListEntry
  ( AliasListEntry (..)
  -- * Smart constructor
  , mkAliasListEntry
  -- * Lenses
  , aleAliasArn
  , aleAliasName
  , aleTargetKeyId
  ) where

import qualified Network.AWS.KMS.Types.AliasArn as Types
import qualified Network.AWS.KMS.Types.AliasName as Types
import qualified Network.AWS.KMS.Types.TargetKeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an alias.
--
-- /See:/ 'mkAliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { aliasArn :: Core.Maybe Types.AliasArn
    -- ^ String that contains the key ARN.
  , aliasName :: Core.Maybe Types.AliasName
    -- ^ String that contains the alias. This value begins with @alias/@ .
  , targetKeyId :: Core.Maybe Types.TargetKeyId
    -- ^ String that contains the key identifier referred to by the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AliasListEntry' value with any optional fields omitted.
mkAliasListEntry
    :: AliasListEntry
mkAliasListEntry
  = AliasListEntry'{aliasArn = Core.Nothing,
                    aliasName = Core.Nothing, targetKeyId = Core.Nothing}

-- | String that contains the key ARN.
--
-- /Note:/ Consider using 'aliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleAliasArn :: Lens.Lens' AliasListEntry (Core.Maybe Types.AliasArn)
aleAliasArn = Lens.field @"aliasArn"
{-# INLINEABLE aleAliasArn #-}
{-# DEPRECATED aliasArn "Use generic-lens or generic-optics with 'aliasArn' instead"  #-}

-- | String that contains the alias. This value begins with @alias/@ .
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleAliasName :: Lens.Lens' AliasListEntry (Core.Maybe Types.AliasName)
aleAliasName = Lens.field @"aliasName"
{-# INLINEABLE aleAliasName #-}
{-# DEPRECATED aliasName "Use generic-lens or generic-optics with 'aliasName' instead"  #-}

-- | String that contains the key identifier referred to by the alias.
--
-- /Note:/ Consider using 'targetKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleTargetKeyId :: Lens.Lens' AliasListEntry (Core.Maybe Types.TargetKeyId)
aleTargetKeyId = Lens.field @"targetKeyId"
{-# INLINEABLE aleTargetKeyId #-}
{-# DEPRECATED targetKeyId "Use generic-lens or generic-optics with 'targetKeyId' instead"  #-}

instance Core.FromJSON AliasListEntry where
        parseJSON
          = Core.withObject "AliasListEntry" Core.$
              \ x ->
                AliasListEntry' Core.<$>
                  (x Core..:? "AliasArn") Core.<*> x Core..:? "AliasName" Core.<*>
                    x Core..:? "TargetKeyId"
