-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfile
  ( QueryArgProfile (..),

    -- * Smart constructor
    mkQueryArgProfile,

    -- * Lenses
    qapQueryArg,
    qapProfileId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfile' smart constructor.
data QueryArgProfile = QueryArgProfile'
  { queryArg :: Lude.Text,
    profileId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryArgProfile' with the minimum fields required to make a request.
--
-- * 'profileId' - ID of profile to use for field-level encryption query argument-profile mapping
-- * 'queryArg' - Query argument for field-level encryption query argument-profile mapping.
mkQueryArgProfile ::
  -- | 'queryArg'
  Lude.Text ->
  -- | 'profileId'
  Lude.Text ->
  QueryArgProfile
mkQueryArgProfile pQueryArg_ pProfileId_ =
  QueryArgProfile' {queryArg = pQueryArg_, profileId = pProfileId_}

-- | Query argument for field-level encryption query argument-profile mapping.
--
-- /Note:/ Consider using 'queryArg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapQueryArg :: Lens.Lens' QueryArgProfile Lude.Text
qapQueryArg = Lens.lens (queryArg :: QueryArgProfile -> Lude.Text) (\s a -> s {queryArg = a} :: QueryArgProfile)
{-# DEPRECATED qapQueryArg "Use generic-lens or generic-optics with 'queryArg' instead." #-}

-- | ID of profile to use for field-level encryption query argument-profile mapping
--
-- /Note:/ Consider using 'profileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapProfileId :: Lens.Lens' QueryArgProfile Lude.Text
qapProfileId = Lens.lens (profileId :: QueryArgProfile -> Lude.Text) (\s a -> s {profileId = a} :: QueryArgProfile)
{-# DEPRECATED qapProfileId "Use generic-lens or generic-optics with 'profileId' instead." #-}

instance Lude.FromXML QueryArgProfile where
  parseXML x =
    QueryArgProfile'
      Lude.<$> (x Lude..@ "QueryArg") Lude.<*> (x Lude..@ "ProfileId")

instance Lude.ToXML QueryArgProfile where
  toXML QueryArgProfile' {..} =
    Lude.mconcat
      ["QueryArg" Lude.@= queryArg, "ProfileId" Lude.@= profileId]
