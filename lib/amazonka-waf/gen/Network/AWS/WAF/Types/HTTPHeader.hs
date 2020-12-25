{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.HTTPHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.HTTPHeader
  ( HTTPHeader (..),

    -- * Smart constructor
    mkHTTPHeader,

    -- * Lenses
    httphName,
    httphValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.HeaderValue as Types
import qualified Network.AWS.WAF.Types.Name as Types

-- | The response from a 'GetSampledRequests' request includes an @HTTPHeader@ complex type that appears as @Headers@ in the response syntax. @HTTPHeader@ contains the names and values of all of the headers that appear in one of the web requests that were returned by @GetSampledRequests@ .
--
-- /See:/ 'mkHTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { -- | The name of one of the headers in the sampled web request.
    name :: Core.Maybe Types.Name,
    -- | The value of one of the headers in the sampled web request.
    value :: Core.Maybe Types.HeaderValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HTTPHeader' value with any optional fields omitted.
mkHTTPHeader ::
  HTTPHeader
mkHTTPHeader =
  HTTPHeader' {name = Core.Nothing, value = Core.Nothing}

-- | The name of one of the headers in the sampled web request.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphName :: Lens.Lens' HTTPHeader (Core.Maybe Types.Name)
httphName = Lens.field @"name"
{-# DEPRECATED httphName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of one of the headers in the sampled web request.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphValue :: Lens.Lens' HTTPHeader (Core.Maybe Types.HeaderValue)
httphValue = Lens.field @"value"
{-# DEPRECATED httphValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON HTTPHeader where
  parseJSON =
    Core.withObject "HTTPHeader" Core.$
      \x ->
        HTTPHeader'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Value")
