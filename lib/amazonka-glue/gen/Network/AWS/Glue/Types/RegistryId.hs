-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryId
  ( RegistryId (..),

    -- * Smart constructor
    mkRegistryId,

    -- * Lenses
    riRegistryName,
    riRegistryARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /See:/ 'mkRegistryId' smart constructor.
data RegistryId = RegistryId'
  { registryName :: Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegistryId' with the minimum fields required to make a request.
--
-- * 'registryARN' - Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
-- * 'registryName' - Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided.
mkRegistryId ::
  RegistryId
mkRegistryId =
  RegistryId'
    { registryName = Lude.Nothing,
      registryARN = Lude.Nothing
    }

-- | Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegistryName :: Lens.Lens' RegistryId (Lude.Maybe Lude.Text)
riRegistryName = Lens.lens (registryName :: RegistryId -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: RegistryId)
{-# DEPRECATED riRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegistryARN :: Lens.Lens' RegistryId (Lude.Maybe Lude.Text)
riRegistryARN = Lens.lens (registryARN :: RegistryId -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: RegistryId)
{-# DEPRECATED riRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

instance Lude.ToJSON RegistryId where
  toJSON RegistryId' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegistryName" Lude..=) Lude.<$> registryName,
            ("RegistryArn" Lude..=) Lude.<$> registryARN
          ]
      )
