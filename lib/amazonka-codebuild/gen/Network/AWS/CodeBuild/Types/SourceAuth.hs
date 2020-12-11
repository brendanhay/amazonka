-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceAuth
  ( SourceAuth (..),

    -- * Smart constructor
    mkSourceAuth,

    -- * Lenses
    saResource,
    saType,
  )
where

import Network.AWS.CodeBuild.Types.SourceAuthType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
-- /See:/ 'mkSourceAuth' smart constructor.
data SourceAuth = SourceAuth'
  { resource :: Lude.Maybe Lude.Text,
    type' :: SourceAuthType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceAuth' with the minimum fields required to make a request.
--
-- * 'resource' - The resource value that applies to the specified authorization type.
-- * 'type'' - The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
mkSourceAuth ::
  -- | 'type''
  SourceAuthType ->
  SourceAuth
mkSourceAuth pType_ =
  SourceAuth' {resource = Lude.Nothing, type' = pType_}

-- | The resource value that applies to the specified authorization type.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saResource :: Lens.Lens' SourceAuth (Lude.Maybe Lude.Text)
saResource = Lens.lens (resource :: SourceAuth -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: SourceAuth)
{-# DEPRECATED saResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saType :: Lens.Lens' SourceAuth SourceAuthType
saType = Lens.lens (type' :: SourceAuth -> SourceAuthType) (\s a -> s {type' = a} :: SourceAuth)
{-# DEPRECATED saType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SourceAuth where
  parseJSON =
    Lude.withObject
      "SourceAuth"
      ( \x ->
          SourceAuth'
            Lude.<$> (x Lude..:? "resource") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON SourceAuth where
  toJSON SourceAuth' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resource" Lude..=) Lude.<$> resource,
            Lude.Just ("type" Lude..= type')
          ]
      )
