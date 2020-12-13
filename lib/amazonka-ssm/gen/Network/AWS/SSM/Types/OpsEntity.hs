{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntity
  ( OpsEntity (..),

    -- * Smart constructor
    mkOpsEntity,

    -- * Lenses
    oeData,
    oeId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsEntityItem

-- | The result of the query.
--
-- /See:/ 'mkOpsEntity' smart constructor.
data OpsEntity = OpsEntity'
  { -- | The data returned by the query.
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (OpsEntityItem)),
    -- | The query ID.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsEntity' with the minimum fields required to make a request.
--
-- * 'data'' - The data returned by the query.
-- * 'id' - The query ID.
mkOpsEntity ::
  OpsEntity
mkOpsEntity = OpsEntity' {data' = Lude.Nothing, id = Lude.Nothing}

-- | The data returned by the query.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeData :: Lens.Lens' OpsEntity (Lude.Maybe (Lude.HashMap Lude.Text (OpsEntityItem)))
oeData = Lens.lens (data' :: OpsEntity -> Lude.Maybe (Lude.HashMap Lude.Text (OpsEntityItem))) (\s a -> s {data' = a} :: OpsEntity)
{-# DEPRECATED oeData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The query ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeId :: Lens.Lens' OpsEntity (Lude.Maybe Lude.Text)
oeId = Lens.lens (id :: OpsEntity -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OpsEntity)
{-# DEPRECATED oeId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON OpsEntity where
  parseJSON =
    Lude.withObject
      "OpsEntity"
      ( \x ->
          OpsEntity'
            Lude.<$> (x Lude..:? "Data" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
      )
