{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionInfo
  ( RevisionInfo (..),

    -- * Smart constructor
    mkRevisionInfo,

    -- * Lenses
    riGenericRevisionInfo,
    riRevisionLocation,
  )
where

import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an application revision.
--
-- /See:/ 'mkRevisionInfo' smart constructor.
data RevisionInfo = RevisionInfo'
  { genericRevisionInfo ::
      Lude.Maybe GenericRevisionInfo,
    revisionLocation :: Lude.Maybe RevisionLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevisionInfo' with the minimum fields required to make a request.
--
-- * 'genericRevisionInfo' - Information about an application revision, including usage details and associated deployment groups.
-- * 'revisionLocation' - Information about the location and type of an application revision.
mkRevisionInfo ::
  RevisionInfo
mkRevisionInfo =
  RevisionInfo'
    { genericRevisionInfo = Lude.Nothing,
      revisionLocation = Lude.Nothing
    }

-- | Information about an application revision, including usage details and associated deployment groups.
--
-- /Note:/ Consider using 'genericRevisionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riGenericRevisionInfo :: Lens.Lens' RevisionInfo (Lude.Maybe GenericRevisionInfo)
riGenericRevisionInfo = Lens.lens (genericRevisionInfo :: RevisionInfo -> Lude.Maybe GenericRevisionInfo) (\s a -> s {genericRevisionInfo = a} :: RevisionInfo)
{-# DEPRECATED riGenericRevisionInfo "Use generic-lens or generic-optics with 'genericRevisionInfo' instead." #-}

-- | Information about the location and type of an application revision.
--
-- /Note:/ Consider using 'revisionLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRevisionLocation :: Lens.Lens' RevisionInfo (Lude.Maybe RevisionLocation)
riRevisionLocation = Lens.lens (revisionLocation :: RevisionInfo -> Lude.Maybe RevisionLocation) (\s a -> s {revisionLocation = a} :: RevisionInfo)
{-# DEPRECATED riRevisionLocation "Use generic-lens or generic-optics with 'revisionLocation' instead." #-}

instance Lude.FromJSON RevisionInfo where
  parseJSON =
    Lude.withObject
      "RevisionInfo"
      ( \x ->
          RevisionInfo'
            Lude.<$> (x Lude..:? "genericRevisionInfo")
            Lude.<*> (x Lude..:? "revisionLocation")
      )
