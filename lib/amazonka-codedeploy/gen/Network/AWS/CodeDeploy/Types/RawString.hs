{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RawString
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RawString
  ( RawString (..),

    -- * Smart constructor
    mkRawString,

    -- * Lenses
    rsContent,
    rsSha256,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.
--
-- /See:/ 'mkRawString' smart constructor.
data RawString = RawString'
  { -- | The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
    content :: Lude.Maybe Lude.Text,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RawString' with the minimum fields required to make a request.
--
-- * 'content' - The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
-- * 'sha256' - The SHA256 hash value of the revision content.
mkRawString ::
  RawString
mkRawString =
  RawString' {content = Lude.Nothing, sha256 = Lude.Nothing}

-- | The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsContent :: Lens.Lens' RawString (Lude.Maybe Lude.Text)
rsContent = Lens.lens (content :: RawString -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: RawString)
{-# DEPRECATED rsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The SHA256 hash value of the revision content.
--
-- /Note:/ Consider using 'sha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSha256 :: Lens.Lens' RawString (Lude.Maybe Lude.Text)
rsSha256 = Lens.lens (sha256 :: RawString -> Lude.Maybe Lude.Text) (\s a -> s {sha256 = a} :: RawString)
{-# DEPRECATED rsSha256 "Use generic-lens or generic-optics with 'sha256' instead." #-}

instance Lude.FromJSON RawString where
  parseJSON =
    Lude.withObject
      "RawString"
      ( \x ->
          RawString'
            Lude.<$> (x Lude..:? "content") Lude.<*> (x Lude..:? "sha256")
      )

instance Lude.ToJSON RawString where
  toJSON RawString' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("content" Lude..=) Lude.<$> content,
            ("sha256" Lude..=) Lude.<$> sha256
          ]
      )
