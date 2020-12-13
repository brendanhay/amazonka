{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of resources that are compliant and the number that are noncompliant. You can specify one or more resource types to get these numbers for each resource type. The maximum number returned is 100.
module Network.AWS.Config.GetComplianceSummaryByResourceType
  ( -- * Creating a request
    GetComplianceSummaryByResourceType (..),
    mkGetComplianceSummaryByResourceType,

    -- ** Request lenses
    gcsbrtResourceTypes,

    -- * Destructuring the response
    GetComplianceSummaryByResourceTypeResponse (..),
    mkGetComplianceSummaryByResourceTypeResponse,

    -- ** Response lenses
    gcsbrtrsComplianceSummariesByResourceType,
    gcsbrtrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkGetComplianceSummaryByResourceType' smart constructor.
newtype GetComplianceSummaryByResourceType = GetComplianceSummaryByResourceType'
  { -- | Specify one or more resource types to get the number of resources that are compliant and the number that are noncompliant for each resource type.
    --
    -- For this request, you can specify an AWS resource type such as @AWS::EC2::Instance@ . You can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
    resourceTypes :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceSummaryByResourceType' with the minimum fields required to make a request.
--
-- * 'resourceTypes' - Specify one or more resource types to get the number of resources that are compliant and the number that are noncompliant for each resource type.
--
-- For this request, you can specify an AWS resource type such as @AWS::EC2::Instance@ . You can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
mkGetComplianceSummaryByResourceType ::
  GetComplianceSummaryByResourceType
mkGetComplianceSummaryByResourceType =
  GetComplianceSummaryByResourceType' {resourceTypes = Lude.Nothing}

-- | Specify one or more resource types to get the number of resources that are compliant and the number that are noncompliant for each resource type.
--
-- For this request, you can specify an AWS resource type such as @AWS::EC2::Instance@ . You can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbrtResourceTypes :: Lens.Lens' GetComplianceSummaryByResourceType (Lude.Maybe [Lude.Text])
gcsbrtResourceTypes = Lens.lens (resourceTypes :: GetComplianceSummaryByResourceType -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: GetComplianceSummaryByResourceType)
{-# DEPRECATED gcsbrtResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

instance Lude.AWSRequest GetComplianceSummaryByResourceType where
  type
    Rs GetComplianceSummaryByResourceType =
      GetComplianceSummaryByResourceTypeResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceSummaryByResourceTypeResponse'
            Lude.<$> ( x Lude..?> "ComplianceSummariesByResourceType"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceSummaryByResourceType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetComplianceSummaryByResourceType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceSummaryByResourceType where
  toJSON GetComplianceSummaryByResourceType' {..} =
    Lude.object
      (Lude.catMaybes [("ResourceTypes" Lude..=) Lude.<$> resourceTypes])

instance Lude.ToPath GetComplianceSummaryByResourceType where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceSummaryByResourceType where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkGetComplianceSummaryByResourceTypeResponse' smart constructor.
data GetComplianceSummaryByResourceTypeResponse = GetComplianceSummaryByResourceTypeResponse'
  { -- | The number of resources that are compliant and the number that are noncompliant. If one or more resource types were provided with the request, the numbers are returned for each resource type. The maximum number returned is 100.
    complianceSummariesByResourceType :: Lude.Maybe [ComplianceSummaryByResourceType],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceSummaryByResourceTypeResponse' with the minimum fields required to make a request.
--
-- * 'complianceSummariesByResourceType' - The number of resources that are compliant and the number that are noncompliant. If one or more resource types were provided with the request, the numbers are returned for each resource type. The maximum number returned is 100.
-- * 'responseStatus' - The response status code.
mkGetComplianceSummaryByResourceTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceSummaryByResourceTypeResponse
mkGetComplianceSummaryByResourceTypeResponse pResponseStatus_ =
  GetComplianceSummaryByResourceTypeResponse'
    { complianceSummariesByResourceType =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of resources that are compliant and the number that are noncompliant. If one or more resource types were provided with the request, the numbers are returned for each resource type. The maximum number returned is 100.
--
-- /Note:/ Consider using 'complianceSummariesByResourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbrtrsComplianceSummariesByResourceType :: Lens.Lens' GetComplianceSummaryByResourceTypeResponse (Lude.Maybe [ComplianceSummaryByResourceType])
gcsbrtrsComplianceSummariesByResourceType = Lens.lens (complianceSummariesByResourceType :: GetComplianceSummaryByResourceTypeResponse -> Lude.Maybe [ComplianceSummaryByResourceType]) (\s a -> s {complianceSummariesByResourceType = a} :: GetComplianceSummaryByResourceTypeResponse)
{-# DEPRECATED gcsbrtrsComplianceSummariesByResourceType "Use generic-lens or generic-optics with 'complianceSummariesByResourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbrtrsResponseStatus :: Lens.Lens' GetComplianceSummaryByResourceTypeResponse Lude.Int
gcsbrtrsResponseStatus = Lens.lens (responseStatus :: GetComplianceSummaryByResourceTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceSummaryByResourceTypeResponse)
{-# DEPRECATED gcsbrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
