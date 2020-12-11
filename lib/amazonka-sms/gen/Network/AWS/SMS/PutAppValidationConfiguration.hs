{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a validation configuration for the specified application.
module Network.AWS.SMS.PutAppValidationConfiguration
  ( -- * Creating a request
    PutAppValidationConfiguration (..),
    mkPutAppValidationConfiguration,

    -- ** Request lenses
    pavcServerGroupValidationConfigurations,
    pavcAppValidationConfigurations,
    pavcAppId,

    -- * Destructuring the response
    PutAppValidationConfigurationResponse (..),
    mkPutAppValidationConfigurationResponse,

    -- ** Response lenses
    pavcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { serverGroupValidationConfigurations ::
      Lude.Maybe
        [ServerGroupValidationConfiguration],
    appValidationConfigurations ::
      Lude.Maybe
        [AppValidationConfiguration],
    appId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'appValidationConfigurations' - The configuration for application validation.
-- * 'serverGroupValidationConfigurations' - The configuration for instance validation.
mkPutAppValidationConfiguration ::
  -- | 'appId'
  Lude.Text ->
  PutAppValidationConfiguration
mkPutAppValidationConfiguration pAppId_ =
  PutAppValidationConfiguration'
    { serverGroupValidationConfigurations =
        Lude.Nothing,
      appValidationConfigurations = Lude.Nothing,
      appId = pAppId_
    }

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcServerGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Lude.Maybe [ServerGroupValidationConfiguration])
pavcServerGroupValidationConfigurations = Lens.lens (serverGroupValidationConfigurations :: PutAppValidationConfiguration -> Lude.Maybe [ServerGroupValidationConfiguration]) (\s a -> s {serverGroupValidationConfigurations = a} :: PutAppValidationConfiguration)
{-# DEPRECATED pavcServerGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead." #-}

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Lude.Maybe [AppValidationConfiguration])
pavcAppValidationConfigurations = Lens.lens (appValidationConfigurations :: PutAppValidationConfiguration -> Lude.Maybe [AppValidationConfiguration]) (\s a -> s {appValidationConfigurations = a} :: PutAppValidationConfiguration)
{-# DEPRECATED pavcAppValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppId :: Lens.Lens' PutAppValidationConfiguration Lude.Text
pavcAppId = Lens.lens (appId :: PutAppValidationConfiguration -> Lude.Text) (\s a -> s {appId = a} :: PutAppValidationConfiguration)
{-# DEPRECATED pavcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest PutAppValidationConfiguration where
  type
    Rs PutAppValidationConfiguration =
      PutAppValidationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutAppValidationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAppValidationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAppValidationConfiguration where
  toJSON PutAppValidationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverGroupValidationConfigurations" Lude..=)
              Lude.<$> serverGroupValidationConfigurations,
            ("appValidationConfigurations" Lude..=)
              Lude.<$> appValidationConfigurations,
            Lude.Just ("appId" Lude..= appId)
          ]
      )

instance Lude.ToPath PutAppValidationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAppValidationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAppValidationConfigurationResponse' smart constructor.
newtype PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
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

-- | Creates a value of 'PutAppValidationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAppValidationConfigurationResponse
mkPutAppValidationConfigurationResponse pResponseStatus_ =
  PutAppValidationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcrsResponseStatus :: Lens.Lens' PutAppValidationConfigurationResponse Lude.Int
pavcrsResponseStatus = Lens.lens (responseStatus :: PutAppValidationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAppValidationConfigurationResponse)
{-# DEPRECATED pavcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
