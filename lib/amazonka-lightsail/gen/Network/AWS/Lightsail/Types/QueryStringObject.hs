{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.QueryStringObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.QueryStringObject
  ( QueryStringObject (..),

    -- * Smart constructor
    mkQueryStringObject,

    -- * Lenses
    qsoOption,
    qsoQueryStringsAllowList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the query string parameters that an Amazon Lightsail content delivery network (CDN) distribution to bases caching on.
--
-- For the query strings that you specify, your distribution caches separate versions of the specified content based on the query string values in viewer requests.
--
-- /See:/ 'mkQueryStringObject' smart constructor.
data QueryStringObject = QueryStringObject'
  { -- | Indicates whether the distribution forwards and caches based on query strings.
    option :: Core.Maybe Core.Bool,
    -- | The specific query strings that the distribution forwards to the origin.
    --
    -- Your distribution will cache content based on the specified query strings.
    -- If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
    queryStringsAllowList :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStringObject' value with any optional fields omitted.
mkQueryStringObject ::
  QueryStringObject
mkQueryStringObject =
  QueryStringObject'
    { option = Core.Nothing,
      queryStringsAllowList = Core.Nothing
    }

-- | Indicates whether the distribution forwards and caches based on query strings.
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsoOption :: Lens.Lens' QueryStringObject (Core.Maybe Core.Bool)
qsoOption = Lens.field @"option"
{-# DEPRECATED qsoOption "Use generic-lens or generic-optics with 'option' instead." #-}

-- | The specific query strings that the distribution forwards to the origin.
--
-- Your distribution will cache content based on the specified query strings.
-- If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
--
-- /Note:/ Consider using 'queryStringsAllowList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsoQueryStringsAllowList :: Lens.Lens' QueryStringObject (Core.Maybe [Types.String])
qsoQueryStringsAllowList = Lens.field @"queryStringsAllowList"
{-# DEPRECATED qsoQueryStringsAllowList "Use generic-lens or generic-optics with 'queryStringsAllowList' instead." #-}

instance Core.FromJSON QueryStringObject where
  toJSON QueryStringObject {..} =
    Core.object
      ( Core.catMaybes
          [ ("option" Core..=) Core.<$> option,
            ("queryStringsAllowList" Core..=) Core.<$> queryStringsAllowList
          ]
      )

instance Core.FromJSON QueryStringObject where
  parseJSON =
    Core.withObject "QueryStringObject" Core.$
      \x ->
        QueryStringObject'
          Core.<$> (x Core..:? "option") Core.<*> (x Core..:? "queryStringsAllowList")
