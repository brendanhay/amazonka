{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a patch baseline.
module Network.AWS.SSM.GetPatchBaseline
  ( -- * Creating a request
    GetPatchBaseline (..),
    mkGetPatchBaseline,

    -- ** Request lenses
    gpbBaselineId,

    -- * Destructuring the response
    GetPatchBaselineResponse (..),
    mkGetPatchBaselineResponse,

    -- ** Response lenses
    gpbrsApprovalRules,
    gpbrsOperatingSystem,
    gpbrsGlobalFilters,
    gpbrsApprovedPatchesComplianceLevel,
    gpbrsRejectedPatchesAction,
    gpbrsApprovedPatches,
    gpbrsApprovedPatchesEnableNonSecurity,
    gpbrsRejectedPatches,
    gpbrsSources,
    gpbrsCreatedDate,
    gpbrsName,
    gpbrsPatchGroups,
    gpbrsModifiedDate,
    gpbrsDescription,
    gpbrsBaselineId,
    gpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetPatchBaseline' smart constructor.
newtype GetPatchBaseline = GetPatchBaseline'
  { -- | The ID of the patch baseline to retrieve.
    baselineId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPatchBaseline' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline to retrieve.
mkGetPatchBaseline ::
  -- | 'baselineId'
  Lude.Text ->
  GetPatchBaseline
mkGetPatchBaseline pBaselineId_ =
  GetPatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline to retrieve.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbBaselineId :: Lens.Lens' GetPatchBaseline Lude.Text
gpbBaselineId = Lens.lens (baselineId :: GetPatchBaseline -> Lude.Text) (\s a -> s {baselineId = a} :: GetPatchBaseline)
{-# DEPRECATED gpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Lude.AWSRequest GetPatchBaseline where
  type Rs GetPatchBaseline = GetPatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPatchBaselineResponse'
            Lude.<$> (x Lude..?> "ApprovalRules")
            Lude.<*> (x Lude..?> "OperatingSystem")
            Lude.<*> (x Lude..?> "GlobalFilters")
            Lude.<*> (x Lude..?> "ApprovedPatchesComplianceLevel")
            Lude.<*> (x Lude..?> "RejectedPatchesAction")
            Lude.<*> (x Lude..?> "ApprovedPatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ApprovedPatchesEnableNonSecurity")
            Lude.<*> (x Lude..?> "RejectedPatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Sources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "PatchGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ModifiedDate")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "BaselineId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetPatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPatchBaseline where
  toJSON GetPatchBaseline' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BaselineId" Lude..= baselineId)])

instance Lude.ToPath GetPatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPatchBaselineResponse' smart constructor.
data GetPatchBaselineResponse = GetPatchBaselineResponse'
  { -- | A set of rules used to include patches in the baseline.
    approvalRules :: Lude.Maybe PatchRuleGroup,
    -- | Returns the operating system specified for the patch baseline.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A set of global filters used to exclude patches from the baseline.
    globalFilters :: Lude.Maybe PatchFilterGroup,
    -- | Returns the specified compliance severity level for approved patches in the patch baseline.
    approvedPatchesComplianceLevel :: Lude.Maybe PatchComplianceLevel,
    -- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
    rejectedPatchesAction :: Lude.Maybe PatchAction,
    -- | A list of explicitly approved patches for the baseline.
    approvedPatches :: Lude.Maybe [Lude.Text],
    -- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Lude.Maybe Lude.Bool,
    -- | A list of explicitly rejected patches for the baseline.
    rejectedPatches :: Lude.Maybe [Lude.Text],
    -- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
    sources :: Lude.Maybe [PatchSource],
    -- | The date the patch baseline was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the patch baseline.
    name :: Lude.Maybe Lude.Text,
    -- | Patch groups included in the patch baseline.
    patchGroups :: Lude.Maybe [Lude.Text],
    -- | The date the patch baseline was last modified.
    modifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the patch baseline.
    description :: Lude.Maybe Lude.Text,
    -- | The ID of the retrieved patch baseline.
    baselineId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'approvalRules' - A set of rules used to include patches in the baseline.
-- * 'operatingSystem' - Returns the operating system specified for the patch baseline.
-- * 'globalFilters' - A set of global filters used to exclude patches from the baseline.
-- * 'approvedPatchesComplianceLevel' - Returns the specified compliance severity level for approved patches in the patch baseline.
-- * 'rejectedPatchesAction' - The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
-- * 'approvedPatches' - A list of explicitly approved patches for the baseline.
-- * 'approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
-- * 'rejectedPatches' - A list of explicitly rejected patches for the baseline.
-- * 'sources' - Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
-- * 'createdDate' - The date the patch baseline was created.
-- * 'name' - The name of the patch baseline.
-- * 'patchGroups' - Patch groups included in the patch baseline.
-- * 'modifiedDate' - The date the patch baseline was last modified.
-- * 'description' - A description of the patch baseline.
-- * 'baselineId' - The ID of the retrieved patch baseline.
-- * 'responseStatus' - The response status code.
mkGetPatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPatchBaselineResponse
mkGetPatchBaselineResponse pResponseStatus_ =
  GetPatchBaselineResponse'
    { approvalRules = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      globalFilters = Lude.Nothing,
      approvedPatchesComplianceLevel = Lude.Nothing,
      rejectedPatchesAction = Lude.Nothing,
      approvedPatches = Lude.Nothing,
      approvedPatchesEnableNonSecurity = Lude.Nothing,
      rejectedPatches = Lude.Nothing,
      sources = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      patchGroups = Lude.Nothing,
      modifiedDate = Lude.Nothing,
      description = Lude.Nothing,
      baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsApprovalRules :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe PatchRuleGroup)
gpbrsApprovalRules = Lens.lens (approvalRules :: GetPatchBaselineResponse -> Lude.Maybe PatchRuleGroup) (\s a -> s {approvalRules = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | Returns the operating system specified for the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsOperatingSystem :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe OperatingSystem)
gpbrsOperatingSystem = Lens.lens (operatingSystem :: GetPatchBaselineResponse -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A set of global filters used to exclude patches from the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsGlobalFilters :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe PatchFilterGroup)
gpbrsGlobalFilters = Lens.lens (globalFilters :: GetPatchBaselineResponse -> Lude.Maybe PatchFilterGroup) (\s a -> s {globalFilters = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | Returns the specified compliance severity level for approved patches in the patch baseline.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsApprovedPatchesComplianceLevel :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe PatchComplianceLevel)
gpbrsApprovedPatchesComplianceLevel = Lens.lens (approvedPatchesComplianceLevel :: GetPatchBaselineResponse -> Lude.Maybe PatchComplianceLevel) (\s a -> s {approvedPatchesComplianceLevel = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

-- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsRejectedPatchesAction :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe PatchAction)
gpbrsRejectedPatchesAction = Lens.lens (rejectedPatchesAction :: GetPatchBaselineResponse -> Lude.Maybe PatchAction) (\s a -> s {rejectedPatchesAction = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsApprovedPatches :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe [Lude.Text])
gpbrsApprovedPatches = Lens.lens (approvedPatches :: GetPatchBaselineResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {approvedPatches = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsApprovedPatchesEnableNonSecurity :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Bool)
gpbrsApprovedPatchesEnableNonSecurity = Lens.lens (approvedPatchesEnableNonSecurity :: GetPatchBaselineResponse -> Lude.Maybe Lude.Bool) (\s a -> s {approvedPatchesEnableNonSecurity = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsRejectedPatches :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe [Lude.Text])
gpbrsRejectedPatches = Lens.lens (rejectedPatches :: GetPatchBaselineResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {rejectedPatches = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsSources :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe [PatchSource])
gpbrsSources = Lens.lens (sources :: GetPatchBaselineResponse -> Lude.Maybe [PatchSource]) (\s a -> s {sources = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | The date the patch baseline was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsCreatedDate :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Timestamp)
gpbrsCreatedDate = Lens.lens (createdDate :: GetPatchBaselineResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsName :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Text)
gpbrsName = Lens.lens (name :: GetPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Patch groups included in the patch baseline.
--
-- /Note:/ Consider using 'patchGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsPatchGroups :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe [Lude.Text])
gpbrsPatchGroups = Lens.lens (patchGroups :: GetPatchBaselineResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {patchGroups = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsPatchGroups "Use generic-lens or generic-optics with 'patchGroups' instead." #-}

-- | The date the patch baseline was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsModifiedDate :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Timestamp)
gpbrsModifiedDate = Lens.lens (modifiedDate :: GetPatchBaselineResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedDate = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsModifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead." #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsDescription :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Text)
gpbrsDescription = Lens.lens (description :: GetPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the retrieved patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsBaselineId :: Lens.Lens' GetPatchBaselineResponse (Lude.Maybe Lude.Text)
gpbrsBaselineId = Lens.lens (baselineId :: GetPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrsResponseStatus :: Lens.Lens' GetPatchBaselineResponse Lude.Int
gpbrsResponseStatus = Lens.lens (responseStatus :: GetPatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPatchBaselineResponse)
{-# DEPRECATED gpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
