{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig
  ( HTTPRequestMethodConditionConfig (..),

    -- * Smart constructor
    mkHTTPRequestMethodConditionConfig,

    -- * Lenses
    httprmccValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an HTTP method condition.
--
-- HTTP defines a set of request methods, also referred to as HTTP verbs. For more information, see the <https://www.iana.org/assignments/http-methods/http-methods.xhtml HTTP Method Registry> . You can also define custom HTTP methods.
--
-- /See:/ 'mkHTTPRequestMethodConditionConfig' smart constructor.
newtype HTTPRequestMethodConditionConfig = HTTPRequestMethodConditionConfig'
  { -- | The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match.
    --
    -- If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
    values :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPRequestMethodConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match.
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
mkHTTPRequestMethodConditionConfig ::
  HTTPRequestMethodConditionConfig
mkHTTPRequestMethodConditionConfig =
  HTTPRequestMethodConditionConfig' {values = Lude.Nothing}

-- | The name of the request method. The maximum size is 40 characters. The allowed characters are A-Z, hyphen (-), and underscore (_). The comparison is case sensitive. Wildcards are not supported; therefore, the method name must be an exact match.
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the HTTP request method. We recommend that you route GET and HEAD requests in the same way, because the response to a HEAD request may be cached.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprmccValues :: Lens.Lens' HTTPRequestMethodConditionConfig (Lude.Maybe [Lude.Text])
httprmccValues = Lens.lens (values :: HTTPRequestMethodConditionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: HTTPRequestMethodConditionConfig)
{-# DEPRECATED httprmccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromXML HTTPRequestMethodConditionConfig where
  parseXML x =
    HTTPRequestMethodConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery HTTPRequestMethodConditionConfig where
  toQuery HTTPRequestMethodConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values)
      ]
