{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.StageKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.StageKey
  ( StageKey (..),

    -- * Smart constructor
    mkStageKey,

    -- * Lenses
    skRestAPIId,
    skStageName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A reference to a unique stage identified in the format @{restApiId}/{stage}@ .
--
-- /See:/ 'mkStageKey' smart constructor.
data StageKey = StageKey'
  { restAPIId :: Lude.Maybe Lude.Text,
    stageName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StageKey' with the minimum fields required to make a request.
--
-- * 'restAPIId' - The string identifier of the associated 'RestApi' .
-- * 'stageName' - The stage name associated with the stage key.
mkStageKey ::
  StageKey
mkStageKey =
  StageKey' {restAPIId = Lude.Nothing, stageName = Lude.Nothing}

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skRestAPIId :: Lens.Lens' StageKey (Lude.Maybe Lude.Text)
skRestAPIId = Lens.lens (restAPIId :: StageKey -> Lude.Maybe Lude.Text) (\s a -> s {restAPIId = a} :: StageKey)
{-# DEPRECATED skRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The stage name associated with the stage key.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skStageName :: Lens.Lens' StageKey (Lude.Maybe Lude.Text)
skStageName = Lens.lens (stageName :: StageKey -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: StageKey)
{-# DEPRECATED skStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.ToJSON StageKey where
  toJSON StageKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("restApiId" Lude..=) Lude.<$> restAPIId,
            ("stageName" Lude..=) Lude.<$> stageName
          ]
      )
