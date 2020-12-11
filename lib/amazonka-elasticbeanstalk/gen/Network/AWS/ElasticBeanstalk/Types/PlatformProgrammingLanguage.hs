-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
  ( PlatformProgrammingLanguage (..),

    -- * Smart constructor
    mkPlatformProgrammingLanguage,

    -- * Lenses
    pplName,
    pplVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A programming language supported by the platform.
--
-- /See:/ 'mkPlatformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { name ::
      Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformProgrammingLanguage' with the minimum fields required to make a request.
--
-- * 'name' - The name of the programming language.
-- * 'version' - The version of the programming language.
mkPlatformProgrammingLanguage ::
  PlatformProgrammingLanguage
mkPlatformProgrammingLanguage =
  PlatformProgrammingLanguage'
    { name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name of the programming language.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplName :: Lens.Lens' PlatformProgrammingLanguage (Lude.Maybe Lude.Text)
pplName = Lens.lens (name :: PlatformProgrammingLanguage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PlatformProgrammingLanguage)
{-# DEPRECATED pplName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the programming language.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplVersion :: Lens.Lens' PlatformProgrammingLanguage (Lude.Maybe Lude.Text)
pplVersion = Lens.lens (version :: PlatformProgrammingLanguage -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: PlatformProgrammingLanguage)
{-# DEPRECATED pplVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromXML PlatformProgrammingLanguage where
  parseXML x =
    PlatformProgrammingLanguage'
      Lude.<$> (x Lude..@? "Name") Lude.<*> (x Lude..@? "Version")
