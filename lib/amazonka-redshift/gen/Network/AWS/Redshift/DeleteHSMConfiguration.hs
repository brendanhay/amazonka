{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteHSMConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHSMConfiguration
  ( -- * Creating a request
    DeleteHSMConfiguration (..),
    mkDeleteHSMConfiguration,

    -- ** Request lenses
    dhsmcHSMConfigurationIdentifier,

    -- * Destructuring the response
    DeleteHSMConfigurationResponse (..),
    mkDeleteHSMConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteHSMConfiguration' smart constructor.
newtype DeleteHSMConfiguration = DeleteHSMConfiguration'
  { -- | The identifier of the Amazon Redshift HSM configuration to be deleted.
    hsmConfigurationIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSMConfiguration' with the minimum fields required to make a request.
--
-- * 'hsmConfigurationIdentifier' - The identifier of the Amazon Redshift HSM configuration to be deleted.
mkDeleteHSMConfiguration ::
  -- | 'hsmConfigurationIdentifier'
  Lude.Text ->
  DeleteHSMConfiguration
mkDeleteHSMConfiguration pHSMConfigurationIdentifier_ =
  DeleteHSMConfiguration'
    { hsmConfigurationIdentifier =
        pHSMConfigurationIdentifier_
    }

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmcHSMConfigurationIdentifier :: Lens.Lens' DeleteHSMConfiguration Lude.Text
dhsmcHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: DeleteHSMConfiguration -> Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: DeleteHSMConfiguration)
{-# DEPRECATED dhsmcHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

instance Lude.AWSRequest DeleteHSMConfiguration where
  type Rs DeleteHSMConfiguration = DeleteHSMConfigurationResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteHSMConfigurationResponse'

instance Lude.ToHeaders DeleteHSMConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteHSMConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHSMConfiguration where
  toQuery DeleteHSMConfiguration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteHsmConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "HsmConfigurationIdentifier" Lude.=: hsmConfigurationIdentifier
      ]

-- | /See:/ 'mkDeleteHSMConfigurationResponse' smart constructor.
data DeleteHSMConfigurationResponse = DeleteHSMConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSMConfigurationResponse' with the minimum fields required to make a request.
mkDeleteHSMConfigurationResponse ::
  DeleteHSMConfigurationResponse
mkDeleteHSMConfigurationResponse = DeleteHSMConfigurationResponse'
