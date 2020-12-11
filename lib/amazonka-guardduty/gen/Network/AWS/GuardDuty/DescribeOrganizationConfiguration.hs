{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the account selected as the delegated administrator for GuardDuty.
module Network.AWS.GuardDuty.DescribeOrganizationConfiguration
  ( -- * Creating a request
    DescribeOrganizationConfiguration (..),
    mkDescribeOrganizationConfiguration,

    -- ** Request lenses
    docDetectorId,

    -- * Destructuring the response
    DescribeOrganizationConfigurationResponse (..),
    mkDescribeOrganizationConfigurationResponse,

    -- ** Response lenses
    docrsDataSources,
    docrsResponseStatus,
    docrsAutoEnable,
    docrsMemberAccountLimitReached,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationConfiguration' smart constructor.
newtype DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { detectorId ::
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

-- | Creates a value of 'DescribeOrganizationConfiguration' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector to retrieve information about the delegated administrator from.
mkDescribeOrganizationConfiguration ::
  -- | 'detectorId'
  Lude.Text ->
  DescribeOrganizationConfiguration
mkDescribeOrganizationConfiguration pDetectorId_ =
  DescribeOrganizationConfiguration' {detectorId = pDetectorId_}

-- | The ID of the detector to retrieve information about the delegated administrator from.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docDetectorId :: Lens.Lens' DescribeOrganizationConfiguration Lude.Text
docDetectorId = Lens.lens (detectorId :: DescribeOrganizationConfiguration -> Lude.Text) (\s a -> s {detectorId = a} :: DescribeOrganizationConfiguration)
{-# DEPRECATED docDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest DescribeOrganizationConfiguration where
  type
    Rs DescribeOrganizationConfiguration =
      DescribeOrganizationConfigurationResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            Lude.<$> (x Lude..?> "dataSources")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "autoEnable")
            Lude.<*> (x Lude..:> "memberAccountLimitReached")
      )

instance Lude.ToHeaders DescribeOrganizationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeOrganizationConfiguration where
  toPath DescribeOrganizationConfiguration' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/admin"]

instance Lude.ToQuery DescribeOrganizationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { dataSources ::
      Lude.Maybe
        OrganizationDataSourceConfigurationsResult,
    responseStatus ::
      Lude.Int,
    autoEnable ::
      Lude.Bool,
    memberAccountLimitReached ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'autoEnable' - Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
-- * 'dataSources' - An object that describes which data sources are enabled automatically for member accounts.
-- * 'memberAccountLimitReached' - Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'autoEnable'
  Lude.Bool ->
  -- | 'memberAccountLimitReached'
  Lude.Bool ->
  DescribeOrganizationConfigurationResponse
mkDescribeOrganizationConfigurationResponse
  pResponseStatus_
  pAutoEnable_
  pMemberAccountLimitReached_ =
    DescribeOrganizationConfigurationResponse'
      { dataSources =
          Lude.Nothing,
        responseStatus = pResponseStatus_,
        autoEnable = pAutoEnable_,
        memberAccountLimitReached =
          pMemberAccountLimitReached_
      }

-- | An object that describes which data sources are enabled automatically for member accounts.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsDataSources :: Lens.Lens' DescribeOrganizationConfigurationResponse (Lude.Maybe OrganizationDataSourceConfigurationsResult)
docrsDataSources = Lens.lens (dataSources :: DescribeOrganizationConfigurationResponse -> Lude.Maybe OrganizationDataSourceConfigurationsResult) (\s a -> s {dataSources = a} :: DescribeOrganizationConfigurationResponse)
{-# DEPRECATED docrsDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Lude.Int
docrsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationConfigurationResponse)
{-# DEPRECATED docrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsAutoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse Lude.Bool
docrsAutoEnable = Lens.lens (autoEnable :: DescribeOrganizationConfigurationResponse -> Lude.Bool) (\s a -> s {autoEnable = a} :: DescribeOrganizationConfigurationResponse)
{-# DEPRECATED docrsAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

-- | Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
--
-- /Note:/ Consider using 'memberAccountLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsMemberAccountLimitReached :: Lens.Lens' DescribeOrganizationConfigurationResponse Lude.Bool
docrsMemberAccountLimitReached = Lens.lens (memberAccountLimitReached :: DescribeOrganizationConfigurationResponse -> Lude.Bool) (\s a -> s {memberAccountLimitReached = a} :: DescribeOrganizationConfigurationResponse)
{-# DEPRECATED docrsMemberAccountLimitReached "Use generic-lens or generic-optics with 'memberAccountLimitReached' instead." #-}
