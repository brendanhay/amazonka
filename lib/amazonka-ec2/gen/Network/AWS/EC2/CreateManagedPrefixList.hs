{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed prefix list. You can specify one or more entries for the prefix list. Each entry consists of a CIDR block and an optional description.
--
-- You must specify the maximum number of entries for the prefix list. The maximum number of entries cannot be changed later.
module Network.AWS.EC2.CreateManagedPrefixList
  ( -- * Creating a request
    CreateManagedPrefixList (..),
    mkCreateManagedPrefixList,

    -- ** Request lenses
    cmplPrefixListName,
    cmplMaxEntries,
    cmplAddressFamily,
    cmplClientToken,
    cmplDryRun,
    cmplEntries,
    cmplTagSpecifications,

    -- * Destructuring the response
    CreateManagedPrefixListResponse (..),
    mkCreateManagedPrefixListResponse,

    -- ** Response lenses
    cmplrrsPrefixList,
    cmplrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateManagedPrefixList' smart constructor.
data CreateManagedPrefixList = CreateManagedPrefixList'
  { -- | A name for the prefix list.
    --
    -- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
    prefixListName :: Types.String,
    -- | The maximum number of entries for the prefix list.
    maxEntries :: Core.Int,
    -- | The IP address type.
    --
    -- Valid Values: @IPv4@ | @IPv6@
    addressFamily :: Types.String,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    --
    -- Constraints: Up to 255 UTF-8 characters in length.
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more entries for the prefix list.
    entries :: Core.Maybe [Types.AddPrefixListEntry],
    -- | The tags to apply to the prefix list during creation.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateManagedPrefixList' value with any optional fields omitted.
mkCreateManagedPrefixList ::
  -- | 'prefixListName'
  Types.String ->
  -- | 'maxEntries'
  Core.Int ->
  -- | 'addressFamily'
  Types.String ->
  CreateManagedPrefixList
mkCreateManagedPrefixList prefixListName maxEntries addressFamily =
  CreateManagedPrefixList'
    { prefixListName,
      maxEntries,
      addressFamily,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      entries = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplPrefixListName :: Lens.Lens' CreateManagedPrefixList Types.String
cmplPrefixListName = Lens.field @"prefixListName"
{-# DEPRECATED cmplPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

-- | The maximum number of entries for the prefix list.
--
-- /Note:/ Consider using 'maxEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplMaxEntries :: Lens.Lens' CreateManagedPrefixList Core.Int
cmplMaxEntries = Lens.field @"maxEntries"
{-# DEPRECATED cmplMaxEntries "Use generic-lens or generic-optics with 'maxEntries' instead." #-}

-- | The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplAddressFamily :: Lens.Lens' CreateManagedPrefixList Types.String
cmplAddressFamily = Lens.field @"addressFamily"
{-# DEPRECATED cmplAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Up to 255 UTF-8 characters in length.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplClientToken :: Lens.Lens' CreateManagedPrefixList (Core.Maybe Types.String)
cmplClientToken = Lens.field @"clientToken"
{-# DEPRECATED cmplClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplDryRun :: Lens.Lens' CreateManagedPrefixList (Core.Maybe Core.Bool)
cmplDryRun = Lens.field @"dryRun"
{-# DEPRECATED cmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more entries for the prefix list.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplEntries :: Lens.Lens' CreateManagedPrefixList (Core.Maybe [Types.AddPrefixListEntry])
cmplEntries = Lens.field @"entries"
{-# DEPRECATED cmplEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The tags to apply to the prefix list during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplTagSpecifications :: Lens.Lens' CreateManagedPrefixList (Core.Maybe [Types.TagSpecification])
cmplTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cmplTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateManagedPrefixList where
  type Rs CreateManagedPrefixList = CreateManagedPrefixListResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateManagedPrefixList")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PrefixListName" prefixListName)
                Core.<> (Core.toQueryValue "MaxEntries" maxEntries)
                Core.<> (Core.toQueryValue "AddressFamily" addressFamily)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Entry" Core.<$> entries)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateManagedPrefixListResponse'
            Core.<$> (x Core..@? "prefixList") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateManagedPrefixListResponse' smart constructor.
data CreateManagedPrefixListResponse = CreateManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe Types.ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateManagedPrefixListResponse' value with any optional fields omitted.
mkCreateManagedPrefixListResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateManagedPrefixListResponse
mkCreateManagedPrefixListResponse responseStatus =
  CreateManagedPrefixListResponse'
    { prefixList = Core.Nothing,
      responseStatus
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrrsPrefixList :: Lens.Lens' CreateManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
cmplrrsPrefixList = Lens.field @"prefixList"
{-# DEPRECATED cmplrrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrrsResponseStatus :: Lens.Lens' CreateManagedPrefixListResponse Core.Int
cmplrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
