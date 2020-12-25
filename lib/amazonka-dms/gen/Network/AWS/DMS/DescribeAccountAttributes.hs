{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the AWS DMS attributes for a customer account. These attributes include AWS DMS quotas for the account and a unique account identifier in a particular DMS region. DMS quotas include a list of resource quotas supported by the account, such as the number of replication instances allowed. The description for each resource quota, includes the quota name, current usage toward that quota, and the quota's maximum value. DMS uses the unique account identifier to name each artifact used by DMS in the given region.
--
-- This command does not take any parameters.
module Network.AWS.DMS.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarrsAccountQuotas,
    daarrsUniqueAccountIdentifier,
    daarrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Core.FromJSON DescribeAccountAttributes where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DescribeAccountAttributes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Core.<$> (x Core..:? "AccountQuotas")
            Core.<*> (x Core..:? "UniqueAccountIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | Account quota information.
    accountQuotas :: Core.Maybe [Types.AccountQuota],
    -- | A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
    uniqueAccountIdentifier :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributesResponse' value with any optional fields omitted.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse responseStatus =
  DescribeAccountAttributesResponse'
    { accountQuotas = Core.Nothing,
      uniqueAccountIdentifier = Core.Nothing,
      responseStatus
    }

-- | Account quota information.
--
-- /Note:/ Consider using 'accountQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAccountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [Types.AccountQuota])
daarrsAccountQuotas = Lens.field @"accountQuotas"
{-# DEPRECATED daarrsAccountQuotas "Use generic-lens or generic-optics with 'accountQuotas' instead." #-}

-- | A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
--
-- /Note:/ Consider using 'uniqueAccountIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsUniqueAccountIdentifier :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe Types.String)
daarrsUniqueAccountIdentifier = Lens.field @"uniqueAccountIdentifier"
{-# DEPRECATED daarrsUniqueAccountIdentifier "Use generic-lens or generic-optics with 'uniqueAccountIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
