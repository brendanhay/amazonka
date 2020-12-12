{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateCACertificateParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateCACertificateParams
  ( UpdateCACertificateParams (..),

    -- * Smart constructor
    mkUpdateCACertificateParams,

    -- * Lenses
    ucacpAction,
  )
where

import Network.AWS.IoT.Types.CACertificateUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
-- /See:/ 'mkUpdateCACertificateParams' smart constructor.
newtype UpdateCACertificateParams = UpdateCACertificateParams'
  { action ::
      CACertificateUpdateAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCACertificateParams' with the minimum fields required to make a request.
--
-- * 'action' - The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
mkUpdateCACertificateParams ::
  -- | 'action'
  CACertificateUpdateAction ->
  UpdateCACertificateParams
mkUpdateCACertificateParams pAction_ =
  UpdateCACertificateParams' {action = pAction_}

-- | The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacpAction :: Lens.Lens' UpdateCACertificateParams CACertificateUpdateAction
ucacpAction = Lens.lens (action :: UpdateCACertificateParams -> CACertificateUpdateAction) (\s a -> s {action = a} :: UpdateCACertificateParams)
{-# DEPRECATED ucacpAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.FromJSON UpdateCACertificateParams where
  parseJSON =
    Lude.withObject
      "UpdateCACertificateParams"
      (\x -> UpdateCACertificateParams' Lude.<$> (x Lude..: "action"))

instance Lude.ToJSON UpdateCACertificateParams where
  toJSON UpdateCACertificateParams' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("action" Lude..= action)])
