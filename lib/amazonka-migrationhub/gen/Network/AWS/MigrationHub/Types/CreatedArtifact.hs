{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.CreatedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.CreatedArtifact
  ( CreatedArtifact (..),

    -- * Smart constructor
    mkCreatedArtifact,

    -- * Lenses
    caDescription,
    caName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).
--
-- /See:/ 'mkCreatedArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { description ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatedArtifact' with the minimum fields required to make a request.
--
-- * 'description' - A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
-- * 'name' - An ARN that uniquely identifies the result of a migration task.
mkCreatedArtifact ::
  -- | 'name'
  Lude.Text ->
  CreatedArtifact
mkCreatedArtifact pName_ =
  CreatedArtifact' {description = Lude.Nothing, name = pName_}

-- | A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreatedArtifact (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreatedArtifact -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatedArtifact)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An ARN that uniquely identifies the result of a migration task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreatedArtifact Lude.Text
caName = Lens.lens (name :: CreatedArtifact -> Lude.Text) (\s a -> s {name = a} :: CreatedArtifact)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CreatedArtifact where
  parseJSON =
    Lude.withObject
      "CreatedArtifact"
      ( \x ->
          CreatedArtifact'
            Lude.<$> (x Lude..:? "Description") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON CreatedArtifact where
  toJSON CreatedArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Name" Lude..= name)
          ]
      )
