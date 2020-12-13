{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfileConfig
  ( QueryArgProfileConfig (..),

    -- * Smart constructor
    mkQueryArgProfileConfig,

    -- * Lenses
    qapcForwardWhenQueryArgProfileIsUnknown,
    qapcQueryArgProfiles,
  )
where

import Network.AWS.CloudFront.Types.QueryArgProfiles
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration for query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { -- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
    forwardWhenQueryArgProfileIsUnknown :: Lude.Bool,
    -- | Profiles specified for query argument-profile mapping for field-level encryption.
    queryArgProfiles :: Lude.Maybe QueryArgProfiles
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryArgProfileConfig' with the minimum fields required to make a request.
--
-- * 'forwardWhenQueryArgProfileIsUnknown' - Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
-- * 'queryArgProfiles' - Profiles specified for query argument-profile mapping for field-level encryption.
mkQueryArgProfileConfig ::
  -- | 'forwardWhenQueryArgProfileIsUnknown'
  Lude.Bool ->
  QueryArgProfileConfig
mkQueryArgProfileConfig pForwardWhenQueryArgProfileIsUnknown_ =
  QueryArgProfileConfig'
    { forwardWhenQueryArgProfileIsUnknown =
        pForwardWhenQueryArgProfileIsUnknown_,
      queryArgProfiles = Lude.Nothing
    }

-- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
--
-- /Note:/ Consider using 'forwardWhenQueryArgProfileIsUnknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcForwardWhenQueryArgProfileIsUnknown :: Lens.Lens' QueryArgProfileConfig Lude.Bool
qapcForwardWhenQueryArgProfileIsUnknown = Lens.lens (forwardWhenQueryArgProfileIsUnknown :: QueryArgProfileConfig -> Lude.Bool) (\s a -> s {forwardWhenQueryArgProfileIsUnknown = a} :: QueryArgProfileConfig)
{-# DEPRECATED qapcForwardWhenQueryArgProfileIsUnknown "Use generic-lens or generic-optics with 'forwardWhenQueryArgProfileIsUnknown' instead." #-}

-- | Profiles specified for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'queryArgProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcQueryArgProfiles :: Lens.Lens' QueryArgProfileConfig (Lude.Maybe QueryArgProfiles)
qapcQueryArgProfiles = Lens.lens (queryArgProfiles :: QueryArgProfileConfig -> Lude.Maybe QueryArgProfiles) (\s a -> s {queryArgProfiles = a} :: QueryArgProfileConfig)
{-# DEPRECATED qapcQueryArgProfiles "Use generic-lens or generic-optics with 'queryArgProfiles' instead." #-}

instance Lude.FromXML QueryArgProfileConfig where
  parseXML x =
    QueryArgProfileConfig'
      Lude.<$> (x Lude..@ "ForwardWhenQueryArgProfileIsUnknown")
      Lude.<*> (x Lude..@? "QueryArgProfiles")

instance Lude.ToXML QueryArgProfileConfig where
  toXML QueryArgProfileConfig' {..} =
    Lude.mconcat
      [ "ForwardWhenQueryArgProfileIsUnknown"
          Lude.@= forwardWhenQueryArgProfileIsUnknown,
        "QueryArgProfiles" Lude.@= queryArgProfiles
      ]
