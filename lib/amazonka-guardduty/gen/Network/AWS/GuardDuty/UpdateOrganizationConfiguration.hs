{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the delegated administrator account with the values provided.
module Network.AWS.GuardDuty.UpdateOrganizationConfiguration
  ( -- * Creating a request
    UpdateOrganizationConfiguration (..),
    mkUpdateOrganizationConfiguration,

    -- ** Request lenses
    uocDataSources,
    uocDetectorId,
    uocAutoEnable,

    -- * Destructuring the response
    UpdateOrganizationConfigurationResponse (..),
    mkUpdateOrganizationConfigurationResponse,

    -- ** Response lenses
    uocrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { dataSources ::
      Lude.Maybe
        OrganizationDataSourceConfigurations,
    detectorId :: Lude.Text,
    autoEnable :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOrganizationConfiguration' with the minimum fields required to make a request.
--
-- * 'autoEnable' - Indicates whether to automatically enable member accounts in the organization.
-- * 'dataSources' - An object describes which data sources will be updated.
-- * 'detectorId' - The ID of the detector to update the delegated administrator for.
mkUpdateOrganizationConfiguration ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'autoEnable'
  Lude.Bool ->
  UpdateOrganizationConfiguration
mkUpdateOrganizationConfiguration pDetectorId_ pAutoEnable_ =
  UpdateOrganizationConfiguration'
    { dataSources = Lude.Nothing,
      detectorId = pDetectorId_,
      autoEnable = pAutoEnable_
    }

-- | An object describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDataSources :: Lens.Lens' UpdateOrganizationConfiguration (Lude.Maybe OrganizationDataSourceConfigurations)
uocDataSources = Lens.lens (dataSources :: UpdateOrganizationConfiguration -> Lude.Maybe OrganizationDataSourceConfigurations) (\s a -> s {dataSources = a} :: UpdateOrganizationConfiguration)
{-# DEPRECATED uocDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The ID of the detector to update the delegated administrator for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDetectorId :: Lens.Lens' UpdateOrganizationConfiguration Lude.Text
uocDetectorId = Lens.lens (detectorId :: UpdateOrganizationConfiguration -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateOrganizationConfiguration)
{-# DEPRECATED uocDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | Indicates whether to automatically enable member accounts in the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocAutoEnable :: Lens.Lens' UpdateOrganizationConfiguration Lude.Bool
uocAutoEnable = Lens.lens (autoEnable :: UpdateOrganizationConfiguration -> Lude.Bool) (\s a -> s {autoEnable = a} :: UpdateOrganizationConfiguration)
{-# DEPRECATED uocAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

instance Lude.AWSRequest UpdateOrganizationConfiguration where
  type
    Rs UpdateOrganizationConfiguration =
      UpdateOrganizationConfigurationResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateOrganizationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateOrganizationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateOrganizationConfiguration where
  toJSON UpdateOrganizationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dataSources" Lude..=) Lude.<$> dataSources,
            Lude.Just ("autoEnable" Lude..= autoEnable)
          ]
      )

instance Lude.ToPath UpdateOrganizationConfiguration where
  toPath UpdateOrganizationConfiguration' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/admin"]

instance Lude.ToQuery UpdateOrganizationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateOrganizationConfigurationResponse' smart constructor.
newtype UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
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

-- | Creates a value of 'UpdateOrganizationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateOrganizationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateOrganizationConfigurationResponse
mkUpdateOrganizationConfigurationResponse pResponseStatus_ =
  UpdateOrganizationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocrsResponseStatus :: Lens.Lens' UpdateOrganizationConfigurationResponse Lude.Int
uocrsResponseStatus = Lens.lens (responseStatus :: UpdateOrganizationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateOrganizationConfigurationResponse)
{-# DEPRECATED uocrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
