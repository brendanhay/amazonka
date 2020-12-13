{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionRevision
  ( ActionRevision (..),

    -- * Smart constructor
    mkActionRevision,

    -- * Lenses
    aCreated,
    aRevisionChangeId,
    aRevisionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the version (or revision) of an action.
--
-- /See:/ 'mkActionRevision' smart constructor.
data ActionRevision = ActionRevision'
  { -- | The date and time when the most recent version of the action was created, in timestamp format.
    created :: Lude.Timestamp,
    -- | The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
    revisionChangeId :: Lude.Text,
    -- | The system-generated unique ID that identifies the revision number of the action.
    revisionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionRevision' with the minimum fields required to make a request.
--
-- * 'created' - The date and time when the most recent version of the action was created, in timestamp format.
-- * 'revisionChangeId' - The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
-- * 'revisionId' - The system-generated unique ID that identifies the revision number of the action.
mkActionRevision ::
  -- | 'created'
  Lude.Timestamp ->
  -- | 'revisionChangeId'
  Lude.Text ->
  -- | 'revisionId'
  Lude.Text ->
  ActionRevision
mkActionRevision pCreated_ pRevisionChangeId_ pRevisionId_ =
  ActionRevision'
    { created = pCreated_,
      revisionChangeId = pRevisionChangeId_,
      revisionId = pRevisionId_
    }

-- | The date and time when the most recent version of the action was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreated :: Lens.Lens' ActionRevision Lude.Timestamp
aCreated = Lens.lens (created :: ActionRevision -> Lude.Timestamp) (\s a -> s {created = a} :: ActionRevision)
{-# DEPRECATED aCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
--
-- /Note:/ Consider using 'revisionChangeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRevisionChangeId :: Lens.Lens' ActionRevision Lude.Text
aRevisionChangeId = Lens.lens (revisionChangeId :: ActionRevision -> Lude.Text) (\s a -> s {revisionChangeId = a} :: ActionRevision)
{-# DEPRECATED aRevisionChangeId "Use generic-lens or generic-optics with 'revisionChangeId' instead." #-}

-- | The system-generated unique ID that identifies the revision number of the action.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRevisionId :: Lens.Lens' ActionRevision Lude.Text
aRevisionId = Lens.lens (revisionId :: ActionRevision -> Lude.Text) (\s a -> s {revisionId = a} :: ActionRevision)
{-# DEPRECATED aRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.FromJSON ActionRevision where
  parseJSON =
    Lude.withObject
      "ActionRevision"
      ( \x ->
          ActionRevision'
            Lude.<$> (x Lude..: "created")
            Lude.<*> (x Lude..: "revisionChangeId")
            Lude.<*> (x Lude..: "revisionId")
      )

instance Lude.ToJSON ActionRevision where
  toJSON ActionRevision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("created" Lude..= created),
            Lude.Just ("revisionChangeId" Lude..= revisionChangeId),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )
