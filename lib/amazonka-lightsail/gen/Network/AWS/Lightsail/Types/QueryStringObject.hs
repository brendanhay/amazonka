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
    qsoQueryStringsAllowList,
    qsoOption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the query string parameters that an Amazon Lightsail content delivery network (CDN) distribution to bases caching on.
--
-- For the query strings that you specify, your distribution caches separate versions of the specified content based on the query string values in viewer requests.
--
-- /See:/ 'mkQueryStringObject' smart constructor.
data QueryStringObject = QueryStringObject'
  { -- | The specific query strings that the distribution forwards to the origin.
    --
    -- Your distribution will cache content based on the specified query strings.
    -- If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
    queryStringsAllowList :: Lude.Maybe [Lude.Text],
    -- | Indicates whether the distribution forwards and caches based on query strings.
    option :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryStringObject' with the minimum fields required to make a request.
--
-- * 'queryStringsAllowList' - The specific query strings that the distribution forwards to the origin.
--
-- Your distribution will cache content based on the specified query strings.
-- If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
-- * 'option' - Indicates whether the distribution forwards and caches based on query strings.
mkQueryStringObject ::
  QueryStringObject
mkQueryStringObject =
  QueryStringObject'
    { queryStringsAllowList = Lude.Nothing,
      option = Lude.Nothing
    }

-- | The specific query strings that the distribution forwards to the origin.
--
-- Your distribution will cache content based on the specified query strings.
-- If the @option@ parameter is true, then your distribution forwards all query strings, regardless of what you specify using the @queryStringsAllowList@ parameter.
--
-- /Note:/ Consider using 'queryStringsAllowList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsoQueryStringsAllowList :: Lens.Lens' QueryStringObject (Lude.Maybe [Lude.Text])
qsoQueryStringsAllowList = Lens.lens (queryStringsAllowList :: QueryStringObject -> Lude.Maybe [Lude.Text]) (\s a -> s {queryStringsAllowList = a} :: QueryStringObject)
{-# DEPRECATED qsoQueryStringsAllowList "Use generic-lens or generic-optics with 'queryStringsAllowList' instead." #-}

-- | Indicates whether the distribution forwards and caches based on query strings.
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsoOption :: Lens.Lens' QueryStringObject (Lude.Maybe Lude.Bool)
qsoOption = Lens.lens (option :: QueryStringObject -> Lude.Maybe Lude.Bool) (\s a -> s {option = a} :: QueryStringObject)
{-# DEPRECATED qsoOption "Use generic-lens or generic-optics with 'option' instead." #-}

instance Lude.FromJSON QueryStringObject where
  parseJSON =
    Lude.withObject
      "QueryStringObject"
      ( \x ->
          QueryStringObject'
            Lude.<$> (x Lude..:? "queryStringsAllowList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "option")
      )

instance Lude.ToJSON QueryStringObject where
  toJSON QueryStringObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryStringsAllowList" Lude..=) Lude.<$> queryStringsAllowList,
            ("option" Lude..=) Lude.<$> option
          ]
      )
