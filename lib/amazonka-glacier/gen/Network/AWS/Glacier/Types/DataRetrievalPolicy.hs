{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalPolicy
  ( DataRetrievalPolicy (..),

    -- * Smart constructor
    mkDataRetrievalPolicy,

    -- * Lenses
    drpRules,
  )
where

import Network.AWS.Glacier.Types.DataRetrievalRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Data retrieval policy.
--
-- /See:/ 'mkDataRetrievalPolicy' smart constructor.
newtype DataRetrievalPolicy = DataRetrievalPolicy'
  { -- | The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
    rules :: Lude.Maybe [DataRetrievalRule]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataRetrievalPolicy' with the minimum fields required to make a request.
--
-- * 'rules' - The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
mkDataRetrievalPolicy ::
  DataRetrievalPolicy
mkDataRetrievalPolicy = DataRetrievalPolicy' {rules = Lude.Nothing}

-- | The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRules :: Lens.Lens' DataRetrievalPolicy (Lude.Maybe [DataRetrievalRule])
drpRules = Lens.lens (rules :: DataRetrievalPolicy -> Lude.Maybe [DataRetrievalRule]) (\s a -> s {rules = a} :: DataRetrievalPolicy)
{-# DEPRECATED drpRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.FromJSON DataRetrievalPolicy where
  parseJSON =
    Lude.withObject
      "DataRetrievalPolicy"
      ( \x ->
          DataRetrievalPolicy'
            Lude.<$> (x Lude..:? "Rules" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DataRetrievalPolicy where
  toJSON DataRetrievalPolicy' {..} =
    Lude.object (Lude.catMaybes [("Rules" Lude..=) Lude.<$> rules])
