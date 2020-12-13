{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutComplianceItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.
--
-- ComplianceType can be one of the following:
--
--     * ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.
--
--
--     * ExecutionType: Specify patch, association, or Custom:@string@ .
--
--
--     * ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.
--
--
--     * Id: The patch, association, or custom compliance ID.
--
--
--     * Title: A title.
--
--
--     * Status: The status of the compliance item. For example, @approved@ for patches, or @Failed@ for associations.
--
--
--     * Severity: A patch severity. For example, @critical@ .
--
--
--     * DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.
--
--
--     * DocumentVersion: An SSM document version number. For example, 4.
--
--
--     * Classification: A patch classification. For example, @security updates@ .
--
--
--     * PatchBaselineId: A patch baseline ID.
--
--
--     * PatchSeverity: A patch severity. For example, @Critical@ .
--
--
--     * PatchState: A patch state. For example, @InstancesWithFailedPatches@ .
--
--
--     * PatchGroup: The name of a patch group.
--
--
--     * InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'
module Network.AWS.SSM.PutComplianceItems
  ( -- * Creating a request
    PutComplianceItems (..),
    mkPutComplianceItems,

    -- ** Request lenses
    pciResourceId,
    pciResourceType,
    pciUploadType,
    pciExecutionSummary,
    pciItems,
    pciComplianceType,
    pciItemContentHash,

    -- * Destructuring the response
    PutComplianceItemsResponse (..),
    mkPutComplianceItemsResponse,

    -- ** Response lenses
    pcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkPutComplianceItems' smart constructor.
data PutComplianceItems = PutComplianceItems'
  { -- | Specify an ID for this resource. For a managed instance, this is the instance ID.
    resourceId :: Lude.Text,
    -- | Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
    resourceType :: Lude.Text,
    -- | The mode for uploading compliance items. You can specify @COMPLETE@ or @PARTIAL@ . In @COMPLETE@ mode, the system overwrites all existing compliance information for the resource. You must provide a full list of compliance items each time you send the request.
    --
    -- In @PARTIAL@ mode, the system overwrites compliance information for a specific association. The association must be configured with @SyncCompliance@ set to @MANUAL@ . By default, all requests use @COMPLETE@ mode.
    uploadType :: Lude.Maybe ComplianceUploadType,
    -- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
    executionSummary :: ComplianceExecutionSummary,
    -- | Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, and so on.
    items :: [ComplianceItemEntry],
    -- | Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
    complianceType :: Lude.Text,
    -- | MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
    itemContentHash :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutComplianceItems' with the minimum fields required to make a request.
--
-- * 'resourceId' - Specify an ID for this resource. For a managed instance, this is the instance ID.
-- * 'resourceType' - Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
-- * 'uploadType' - The mode for uploading compliance items. You can specify @COMPLETE@ or @PARTIAL@ . In @COMPLETE@ mode, the system overwrites all existing compliance information for the resource. You must provide a full list of compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a specific association. The association must be configured with @SyncCompliance@ set to @MANUAL@ . By default, all requests use @COMPLETE@ mode.
-- * 'executionSummary' - A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
-- * 'items' - Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, and so on.
-- * 'complianceType' - Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
-- * 'itemContentHash' - MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
mkPutComplianceItems ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'executionSummary'
  ComplianceExecutionSummary ->
  -- | 'complianceType'
  Lude.Text ->
  PutComplianceItems
mkPutComplianceItems
  pResourceId_
  pResourceType_
  pExecutionSummary_
  pComplianceType_ =
    PutComplianceItems'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        uploadType = Lude.Nothing,
        executionSummary = pExecutionSummary_,
        items = Lude.mempty,
        complianceType = pComplianceType_,
        itemContentHash = Lude.Nothing
      }

-- | Specify an ID for this resource. For a managed instance, this is the instance ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciResourceId :: Lens.Lens' PutComplianceItems Lude.Text
pciResourceId = Lens.lens (resourceId :: PutComplianceItems -> Lude.Text) (\s a -> s {resourceId = a} :: PutComplianceItems)
{-# DEPRECATED pciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciResourceType :: Lens.Lens' PutComplianceItems Lude.Text
pciResourceType = Lens.lens (resourceType :: PutComplianceItems -> Lude.Text) (\s a -> s {resourceType = a} :: PutComplianceItems)
{-# DEPRECATED pciResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The mode for uploading compliance items. You can specify @COMPLETE@ or @PARTIAL@ . In @COMPLETE@ mode, the system overwrites all existing compliance information for the resource. You must provide a full list of compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a specific association. The association must be configured with @SyncCompliance@ set to @MANUAL@ . By default, all requests use @COMPLETE@ mode.
--
-- /Note:/ Consider using 'uploadType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciUploadType :: Lens.Lens' PutComplianceItems (Lude.Maybe ComplianceUploadType)
pciUploadType = Lens.lens (uploadType :: PutComplianceItems -> Lude.Maybe ComplianceUploadType) (\s a -> s {uploadType = a} :: PutComplianceItems)
{-# DEPRECATED pciUploadType "Use generic-lens or generic-optics with 'uploadType' instead." #-}

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciExecutionSummary :: Lens.Lens' PutComplianceItems ComplianceExecutionSummary
pciExecutionSummary = Lens.lens (executionSummary :: PutComplianceItems -> ComplianceExecutionSummary) (\s a -> s {executionSummary = a} :: PutComplianceItems)
{-# DEPRECATED pciExecutionSummary "Use generic-lens or generic-optics with 'executionSummary' instead." #-}

-- | Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, and so on.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciItems :: Lens.Lens' PutComplianceItems [ComplianceItemEntry]
pciItems = Lens.lens (items :: PutComplianceItems -> [ComplianceItemEntry]) (\s a -> s {items = a} :: PutComplianceItems)
{-# DEPRECATED pciItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciComplianceType :: Lens.Lens' PutComplianceItems Lude.Text
pciComplianceType = Lens.lens (complianceType :: PutComplianceItems -> Lude.Text) (\s a -> s {complianceType = a} :: PutComplianceItems)
{-# DEPRECATED pciComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
--
-- /Note:/ Consider using 'itemContentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciItemContentHash :: Lens.Lens' PutComplianceItems (Lude.Maybe Lude.Text)
pciItemContentHash = Lens.lens (itemContentHash :: PutComplianceItems -> Lude.Maybe Lude.Text) (\s a -> s {itemContentHash = a} :: PutComplianceItems)
{-# DEPRECATED pciItemContentHash "Use generic-lens or generic-optics with 'itemContentHash' instead." #-}

instance Lude.AWSRequest PutComplianceItems where
  type Rs PutComplianceItems = PutComplianceItemsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutComplianceItemsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutComplianceItems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.PutComplianceItems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutComplianceItems where
  toJSON PutComplianceItems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType),
            ("UploadType" Lude..=) Lude.<$> uploadType,
            Lude.Just ("ExecutionSummary" Lude..= executionSummary),
            Lude.Just ("Items" Lude..= items),
            Lude.Just ("ComplianceType" Lude..= complianceType),
            ("ItemContentHash" Lude..=) Lude.<$> itemContentHash
          ]
      )

instance Lude.ToPath PutComplianceItems where
  toPath = Lude.const "/"

instance Lude.ToQuery PutComplianceItems where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutComplianceItemsResponse' smart constructor.
newtype PutComplianceItemsResponse = PutComplianceItemsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutComplianceItemsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutComplianceItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutComplianceItemsResponse
mkPutComplianceItemsResponse pResponseStatus_ =
  PutComplianceItemsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcirsResponseStatus :: Lens.Lens' PutComplianceItemsResponse Lude.Int
pcirsResponseStatus = Lens.lens (responseStatus :: PutComplianceItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutComplianceItemsResponse)
{-# DEPRECATED pcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
