{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
  ( EnvironmentLink (..),

    -- * Smart constructor
    mkEnvironmentLink,

    -- * Lenses
    elLinkName,
    elEnvironmentName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /See:/ 'mkEnvironmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { linkName ::
      Lude.Maybe Lude.Text,
    environmentName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentLink' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the linked environment (the dependency).
-- * 'linkName' - The name of the link.
mkEnvironmentLink ::
  EnvironmentLink
mkEnvironmentLink =
  EnvironmentLink'
    { linkName = Lude.Nothing,
      environmentName = Lude.Nothing
    }

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLinkName :: Lens.Lens' EnvironmentLink (Lude.Maybe Lude.Text)
elLinkName = Lens.lens (linkName :: EnvironmentLink -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: EnvironmentLink)
{-# DEPRECATED elLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The name of the linked environment (the dependency).
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elEnvironmentName :: Lens.Lens' EnvironmentLink (Lude.Maybe Lude.Text)
elEnvironmentName = Lens.lens (environmentName :: EnvironmentLink -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: EnvironmentLink)
{-# DEPRECATED elEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Lude.FromXML EnvironmentLink where
  parseXML x =
    EnvironmentLink'
      Lude.<$> (x Lude..@? "LinkName") Lude.<*> (x Lude..@? "EnvironmentName")
