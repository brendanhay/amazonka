{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HeaderObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HeaderObject
  ( HeaderObject (..),

    -- * Smart constructor
    mkHeaderObject,

    -- * Lenses
    hoHeadersAllowList,
    hoOption,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ForwardValues
import Network.AWS.Lightsail.Types.HeaderEnum
import qualified Network.AWS.Prelude as Lude

-- | Describes the request headers that a Lightsail distribution bases caching on.
--
-- For the headers that you specify, your distribution caches separate versions of the specified content based on the header values in viewer requests. For example, suppose viewer requests for @logo.jpg@ contain a custom @product@ header that has a value of either @acme@ or @apex@ , and you configure your distribution to cache your content based on values in the @product@ header. Your distribution forwards the @product@ header to the origin and caches the response from the origin once for each header value.
--
-- /See:/ 'mkHeaderObject' smart constructor.
data HeaderObject = HeaderObject'
  { headersAllowList ::
      Lude.Maybe [HeaderEnum],
    option :: Lude.Maybe ForwardValues
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HeaderObject' with the minimum fields required to make a request.
--
-- * 'headersAllowList' - The specific headers to forward to your distribution's origin.
-- * 'option' - The headers that you want your distribution to forward to your origin and base caching on.
--
-- You can configure your distribution to do one of the following:
--
--     * __@all@ __ - Forward all headers to your origin.
--
--
--     * __@none@ __ - Forward only the default headers.
--
--
--     * __@allow-list@ __ - Forward only the headers you specify using the @headersAllowList@ parameter.
mkHeaderObject ::
  HeaderObject
mkHeaderObject =
  HeaderObject'
    { headersAllowList = Lude.Nothing,
      option = Lude.Nothing
    }

-- | The specific headers to forward to your distribution's origin.
--
-- /Note:/ Consider using 'headersAllowList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoHeadersAllowList :: Lens.Lens' HeaderObject (Lude.Maybe [HeaderEnum])
hoHeadersAllowList = Lens.lens (headersAllowList :: HeaderObject -> Lude.Maybe [HeaderEnum]) (\s a -> s {headersAllowList = a} :: HeaderObject)
{-# DEPRECATED hoHeadersAllowList "Use generic-lens or generic-optics with 'headersAllowList' instead." #-}

-- | The headers that you want your distribution to forward to your origin and base caching on.
--
-- You can configure your distribution to do one of the following:
--
--     * __@all@ __ - Forward all headers to your origin.
--
--
--     * __@none@ __ - Forward only the default headers.
--
--
--     * __@allow-list@ __ - Forward only the headers you specify using the @headersAllowList@ parameter.
--
--
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoOption :: Lens.Lens' HeaderObject (Lude.Maybe ForwardValues)
hoOption = Lens.lens (option :: HeaderObject -> Lude.Maybe ForwardValues) (\s a -> s {option = a} :: HeaderObject)
{-# DEPRECATED hoOption "Use generic-lens or generic-optics with 'option' instead." #-}

instance Lude.FromJSON HeaderObject where
  parseJSON =
    Lude.withObject
      "HeaderObject"
      ( \x ->
          HeaderObject'
            Lude.<$> (x Lude..:? "headersAllowList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "option")
      )

instance Lude.ToJSON HeaderObject where
  toJSON HeaderObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("headersAllowList" Lude..=) Lude.<$> headersAllowList,
            ("option" Lude..=) Lude.<$> option
          ]
      )
