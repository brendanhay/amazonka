{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.CurrentRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.CurrentRevision
  ( CurrentRevision (..),

    -- * Smart constructor
    mkCurrentRevision,

    -- * Lenses
    crRevisionSummary,
    crCreated,
    crRevision,
    crChangeIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a current revision.
--
-- /See:/ 'mkCurrentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { revisionSummary ::
      Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    revision :: Lude.Text,
    changeIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CurrentRevision' with the minimum fields required to make a request.
--
-- * 'changeIdentifier' - The change identifier for the current revision.
-- * 'created' - The date and time when the most recent revision of the artifact was created, in timestamp format.
-- * 'revision' - The revision ID of the current version of an artifact.
-- * 'revisionSummary' - The summary of the most recent revision of the artifact.
mkCurrentRevision ::
  -- | 'revision'
  Lude.Text ->
  -- | 'changeIdentifier'
  Lude.Text ->
  CurrentRevision
mkCurrentRevision pRevision_ pChangeIdentifier_ =
  CurrentRevision'
    { revisionSummary = Lude.Nothing,
      created = Lude.Nothing,
      revision = pRevision_,
      changeIdentifier = pChangeIdentifier_
    }

-- | The summary of the most recent revision of the artifact.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevisionSummary :: Lens.Lens' CurrentRevision (Lude.Maybe Lude.Text)
crRevisionSummary = Lens.lens (revisionSummary :: CurrentRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionSummary = a} :: CurrentRevision)
{-# DEPRECATED crRevisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead." #-}

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreated :: Lens.Lens' CurrentRevision (Lude.Maybe Lude.Timestamp)
crCreated = Lens.lens (created :: CurrentRevision -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: CurrentRevision)
{-# DEPRECATED crCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The revision ID of the current version of an artifact.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevision :: Lens.Lens' CurrentRevision Lude.Text
crRevision = Lens.lens (revision :: CurrentRevision -> Lude.Text) (\s a -> s {revision = a} :: CurrentRevision)
{-# DEPRECATED crRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The change identifier for the current revision.
--
-- /Note:/ Consider using 'changeIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crChangeIdentifier :: Lens.Lens' CurrentRevision Lude.Text
crChangeIdentifier = Lens.lens (changeIdentifier :: CurrentRevision -> Lude.Text) (\s a -> s {changeIdentifier = a} :: CurrentRevision)
{-# DEPRECATED crChangeIdentifier "Use generic-lens or generic-optics with 'changeIdentifier' instead." #-}

instance Lude.ToJSON CurrentRevision where
  toJSON CurrentRevision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("revisionSummary" Lude..=) Lude.<$> revisionSummary,
            ("created" Lude..=) Lude.<$> created,
            Lude.Just ("revision" Lude..= revision),
            Lude.Just ("changeIdentifier" Lude..= changeIdentifier)
          ]
      )
