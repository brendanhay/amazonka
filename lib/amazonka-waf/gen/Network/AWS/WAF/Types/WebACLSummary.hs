{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WebACLSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACLSummary
  ( WebACLSummary (..),

    -- * Smart constructor
    mkWebACLSummary,

    -- * Lenses
    waclsWebACLId,
    waclsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

-- | Contains the identifier and the name or description of the 'WebACL' .
--
-- /See:/ 'mkWebACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
  { -- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
    --
    -- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
    webACLId :: Types.ResourceId,
    -- | A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
    name :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebACLSummary' value with any optional fields omitted.
mkWebACLSummary ::
  -- | 'webACLId'
  Types.ResourceId ->
  -- | 'name'
  Types.ResourceName ->
  WebACLSummary
mkWebACLSummary webACLId name = WebACLSummary' {webACLId, name}

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclsWebACLId :: Lens.Lens' WebACLSummary Types.ResourceId
waclsWebACLId = Lens.field @"webACLId"
{-# DEPRECATED waclsWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclsName :: Lens.Lens' WebACLSummary Types.ResourceName
waclsName = Lens.field @"name"
{-# DEPRECATED waclsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON WebACLSummary where
  parseJSON =
    Core.withObject "WebACLSummary" Core.$
      \x ->
        WebACLSummary'
          Core.<$> (x Core..: "WebACLId") Core.<*> (x Core..: "Name")
