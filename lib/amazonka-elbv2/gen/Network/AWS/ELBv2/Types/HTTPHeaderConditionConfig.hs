{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig
  ( HTTPHeaderConditionConfig (..),

    -- * Smart constructor
    mkHTTPHeaderConditionConfig,

    -- * Lenses
    httphccValues,
    httphccHTTPHeaderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an HTTP header condition.
--
-- There is a set of standard HTTP header fields. You can also define custom HTTP header fields.
--
-- /See:/ 'mkHTTPHeaderConditionConfig' smart constructor.
data HTTPHeaderConditionConfig = HTTPHeaderConditionConfig'
  { -- | One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
    --
    -- If the same header appears multiple times in the request, we search them in order until a match is found.
    -- If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
    values :: Lude.Maybe [Lude.Text],
    -- | The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported.
    --
    -- You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
    hTTPHeaderName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPHeaderConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them in order until a match is found.
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
-- * 'hTTPHeaderName' - The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported.
--
-- You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
mkHTTPHeaderConditionConfig ::
  HTTPHeaderConditionConfig
mkHTTPHeaderConditionConfig =
  HTTPHeaderConditionConfig'
    { values = Lude.Nothing,
      hTTPHeaderName = Lude.Nothing
    }

-- | One or more strings to compare against the value of the HTTP header. The maximum size of each string is 128 characters. The comparison strings are case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them in order until a match is found.
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the value of the HTTP header. To require that all of the strings are a match, create one condition per string.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphccValues :: Lens.Lens' HTTPHeaderConditionConfig (Lude.Maybe [Lude.Text])
httphccValues = Lens.lens (values :: HTTPHeaderConditionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: HTTPHeaderConditionConfig)
{-# DEPRECATED httphccValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the HTTP header field. The maximum size is 40 characters. The header name is case insensitive. The allowed characters are specified by RFC 7230. Wildcards are not supported.
--
-- You can't use an HTTP header condition to specify the host header. Use 'HostHeaderConditionConfig' to specify a host header condition.
--
-- /Note:/ Consider using 'hTTPHeaderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphccHTTPHeaderName :: Lens.Lens' HTTPHeaderConditionConfig (Lude.Maybe Lude.Text)
httphccHTTPHeaderName = Lens.lens (hTTPHeaderName :: HTTPHeaderConditionConfig -> Lude.Maybe Lude.Text) (\s a -> s {hTTPHeaderName = a} :: HTTPHeaderConditionConfig)
{-# DEPRECATED httphccHTTPHeaderName "Use generic-lens or generic-optics with 'hTTPHeaderName' instead." #-}

instance Lude.FromXML HTTPHeaderConditionConfig where
  parseXML x =
    HTTPHeaderConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "HttpHeaderName")

instance Lude.ToQuery HTTPHeaderConditionConfig where
  toQuery HTTPHeaderConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "HttpHeaderName" Lude.=: hTTPHeaderName
      ]
