-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionNumber
  ( SchemaVersionNumber (..),

    -- * Smart constructor
    mkSchemaVersionNumber,

    -- * Lenses
    svnVersionNumber,
    svnLatestVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkSchemaVersionNumber' smart constructor.
data SchemaVersionNumber = SchemaVersionNumber'
  { versionNumber ::
      Lude.Maybe Lude.Natural,
    latestVersion :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaVersionNumber' with the minimum fields required to make a request.
--
-- * 'latestVersion' - Undocumented field.
-- * 'versionNumber' - Undocumented field.
mkSchemaVersionNumber ::
  SchemaVersionNumber
mkSchemaVersionNumber =
  SchemaVersionNumber'
    { versionNumber = Lude.Nothing,
      latestVersion = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVersionNumber :: Lens.Lens' SchemaVersionNumber (Lude.Maybe Lude.Natural)
svnVersionNumber = Lens.lens (versionNumber :: SchemaVersionNumber -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: SchemaVersionNumber)
{-# DEPRECATED svnVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnLatestVersion :: Lens.Lens' SchemaVersionNumber (Lude.Maybe Lude.Bool)
svnLatestVersion = Lens.lens (latestVersion :: SchemaVersionNumber -> Lude.Maybe Lude.Bool) (\s a -> s {latestVersion = a} :: SchemaVersionNumber)
{-# DEPRECATED svnLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

instance Lude.ToJSON SchemaVersionNumber where
  toJSON SchemaVersionNumber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionNumber" Lude..=) Lude.<$> versionNumber,
            ("LatestVersion" Lude..=) Lude.<$> latestVersion
          ]
      )
