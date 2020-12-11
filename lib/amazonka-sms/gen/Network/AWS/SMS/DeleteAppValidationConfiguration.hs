{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Network.AWS.SMS.DeleteAppValidationConfiguration
  ( -- * Creating a request
    DeleteAppValidationConfiguration (..),
    mkDeleteAppValidationConfiguration,

    -- ** Request lenses
    davcAppId,

    -- * Destructuring the response
    DeleteAppValidationConfigurationResponse (..),
    mkDeleteAppValidationConfigurationResponse,

    -- ** Response lenses
    davcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteAppValidationConfiguration' smart constructor.
newtype DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { appId ::
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

-- | Creates a value of 'DeleteAppValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkDeleteAppValidationConfiguration ::
  -- | 'appId'
  Lude.Text ->
  DeleteAppValidationConfiguration
mkDeleteAppValidationConfiguration pAppId_ =
  DeleteAppValidationConfiguration' {appId = pAppId_}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcAppId :: Lens.Lens' DeleteAppValidationConfiguration Lude.Text
davcAppId = Lens.lens (appId :: DeleteAppValidationConfiguration -> Lude.Text) (\s a -> s {appId = a} :: DeleteAppValidationConfiguration)
{-# DEPRECATED davcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest DeleteAppValidationConfiguration where
  type
    Rs DeleteAppValidationConfiguration =
      DeleteAppValidationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAppValidationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAppValidationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAppValidationConfiguration where
  toJSON DeleteAppValidationConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("appId" Lude..= appId)])

instance Lude.ToPath DeleteAppValidationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAppValidationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppValidationConfigurationResponse' smart constructor.
newtype DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppValidationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAppValidationConfigurationResponse
mkDeleteAppValidationConfigurationResponse pResponseStatus_ =
  DeleteAppValidationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcrsResponseStatus :: Lens.Lens' DeleteAppValidationConfigurationResponse Lude.Int
davcrsResponseStatus = Lens.lens (responseStatus :: DeleteAppValidationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAppValidationConfigurationResponse)
{-# DEPRECATED davcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
