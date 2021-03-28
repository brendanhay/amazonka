{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.SupportService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.SupportService
  ( SupportService (..)
  -- * Smart constructor
  , mkSupportService
  -- * Lenses
  , ssCategories
  , ssCode
  , ssName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.Category as Types
import qualified Network.AWS.Support.Types.Code as Types
import qualified Network.AWS.Support.Types.Name as Types

-- | Information about an AWS service returned by the 'DescribeServices' operation.
--
-- /See:/ 'mkSupportService' smart constructor.
data SupportService = SupportService'
  { categories :: Core.Maybe [Types.Category]
    -- ^ A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
  , code :: Core.Maybe Types.Code
    -- ^ The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name for an AWS service. The @code@ element contains the corresponding code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SupportService' value with any optional fields omitted.
mkSupportService
    :: SupportService
mkSupportService
  = SupportService'{categories = Core.Nothing, code = Core.Nothing,
                    name = Core.Nothing}

-- | A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
--
-- /Note:/ Consider using 'categories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCategories :: Lens.Lens' SupportService (Core.Maybe [Types.Category])
ssCategories = Lens.field @"categories"
{-# INLINEABLE ssCategories #-}
{-# DEPRECATED categories "Use generic-lens or generic-optics with 'categories' instead"  #-}

-- | The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCode :: Lens.Lens' SupportService (Core.Maybe Types.Code)
ssCode = Lens.field @"code"
{-# INLINEABLE ssCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The friendly name for an AWS service. The @code@ element contains the corresponding code.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' SupportService (Core.Maybe Types.Name)
ssName = Lens.field @"name"
{-# INLINEABLE ssName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON SupportService where
        parseJSON
          = Core.withObject "SupportService" Core.$
              \ x ->
                SupportService' Core.<$>
                  (x Core..:? "categories") Core.<*> x Core..:? "code" Core.<*>
                    x Core..:? "name"
