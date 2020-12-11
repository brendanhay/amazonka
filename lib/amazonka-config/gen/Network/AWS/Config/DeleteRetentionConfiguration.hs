{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention configuration.
module Network.AWS.Config.DeleteRetentionConfiguration
  ( -- * Creating a request
    DeleteRetentionConfiguration (..),
    mkDeleteRetentionConfiguration,

    -- ** Request lenses
    drcRetentionConfigurationName,

    -- * Destructuring the response
    DeleteRetentionConfigurationResponse (..),
    mkDeleteRetentionConfigurationResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRetentionConfiguration' smart constructor.
newtype DeleteRetentionConfiguration = DeleteRetentionConfiguration'
  { retentionConfigurationName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRetentionConfiguration' with the minimum fields required to make a request.
--
-- * 'retentionConfigurationName' - The name of the retention configuration to delete.
mkDeleteRetentionConfiguration ::
  -- | 'retentionConfigurationName'
  Lude.Text ->
  DeleteRetentionConfiguration
mkDeleteRetentionConfiguration pRetentionConfigurationName_ =
  DeleteRetentionConfiguration'
    { retentionConfigurationName =
        pRetentionConfigurationName_
    }

-- | The name of the retention configuration to delete.
--
-- /Note:/ Consider using 'retentionConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRetentionConfigurationName :: Lens.Lens' DeleteRetentionConfiguration Lude.Text
drcRetentionConfigurationName = Lens.lens (retentionConfigurationName :: DeleteRetentionConfiguration -> Lude.Text) (\s a -> s {retentionConfigurationName = a} :: DeleteRetentionConfiguration)
{-# DEPRECATED drcRetentionConfigurationName "Use generic-lens or generic-optics with 'retentionConfigurationName' instead." #-}

instance Lude.AWSRequest DeleteRetentionConfiguration where
  type
    Rs DeleteRetentionConfiguration =
      DeleteRetentionConfigurationResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteRetentionConfigurationResponse'

instance Lude.ToHeaders DeleteRetentionConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteRetentionConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRetentionConfiguration where
  toJSON DeleteRetentionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("RetentionConfigurationName" Lude..= retentionConfigurationName)
          ]
      )

instance Lude.ToPath DeleteRetentionConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRetentionConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRetentionConfigurationResponse' smart constructor.
data DeleteRetentionConfigurationResponse = DeleteRetentionConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRetentionConfigurationResponse' with the minimum fields required to make a request.
mkDeleteRetentionConfigurationResponse ::
  DeleteRetentionConfigurationResponse
mkDeleteRetentionConfigurationResponse =
  DeleteRetentionConfigurationResponse'
