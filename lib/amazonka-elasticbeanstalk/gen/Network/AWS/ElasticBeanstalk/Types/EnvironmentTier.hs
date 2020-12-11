-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
  ( EnvironmentTier (..),

    -- * Smart constructor
    mkEnvironmentTier,

    -- * Lenses
    etName,
    etVersion,
    etType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of an environment tier
--
-- /See:/ 'mkEnvironmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
  { name ::
      Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentTier' with the minimum fields required to make a request.
--
-- * 'name' - The name of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @WebServer@
--
--
--     * For /Worker tier/ – @Worker@
--
--
-- * 'type'' - The type of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @Standard@
--
--
--     * For /Worker tier/ – @SQS/HTTP@
--
--
-- * 'version' - The version of this environment tier. When you don't set a value to it, Elastic Beanstalk uses the latest compatible worker tier version.
mkEnvironmentTier ::
  EnvironmentTier
mkEnvironmentTier =
  EnvironmentTier'
    { name = Lude.Nothing,
      version = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The name of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @WebServer@
--
--
--     * For /Worker tier/ – @Worker@
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etName :: Lens.Lens' EnvironmentTier (Lude.Maybe Lude.Text)
etName = Lens.lens (name :: EnvironmentTier -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EnvironmentTier)
{-# DEPRECATED etName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of this environment tier. When you don't set a value to it, Elastic Beanstalk uses the latest compatible worker tier version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etVersion :: Lens.Lens' EnvironmentTier (Lude.Maybe Lude.Text)
etVersion = Lens.lens (version :: EnvironmentTier -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: EnvironmentTier)
{-# DEPRECATED etVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The type of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @Standard@
--
--
--     * For /Worker tier/ – @SQS/HTTP@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etType :: Lens.Lens' EnvironmentTier (Lude.Maybe Lude.Text)
etType = Lens.lens (type' :: EnvironmentTier -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: EnvironmentTier)
{-# DEPRECATED etType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML EnvironmentTier where
  parseXML x =
    EnvironmentTier'
      Lude.<$> (x Lude..@? "Name")
      Lude.<*> (x Lude..@? "Version")
      Lude.<*> (x Lude..@? "Type")

instance Lude.ToQuery EnvironmentTier where
  toQuery EnvironmentTier' {..} =
    Lude.mconcat
      [ "Name" Lude.=: name,
        "Version" Lude.=: version,
        "Type" Lude.=: type'
      ]
