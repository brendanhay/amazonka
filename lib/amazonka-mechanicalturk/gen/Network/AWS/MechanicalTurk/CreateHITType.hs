{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateHITType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHITType@ operation creates a new HIT type. This operation allows you to define a standard set of HIT properties to use when creating HITs. If you register a HIT type with values that match an existing HIT type, the HIT type ID of the existing type will be returned.
module Network.AWS.MechanicalTurk.CreateHITType
  ( -- * Creating a request
    CreateHITType (..),
    mkCreateHITType,

    -- ** Request lenses
    chittAssignmentDurationInSeconds,
    chittReward,
    chittTitle,
    chittDescription,
    chittAutoApprovalDelayInSeconds,
    chittKeywords,
    chittQualificationRequirements,

    -- * Destructuring the response
    CreateHITTypeResponse (..),
    mkCreateHITTypeResponse,

    -- ** Response lenses
    chittrrsHITTypeId,
    chittrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateHITType' smart constructor.
data CreateHITType = CreateHITType'
  { -- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
    assignmentDurationInSeconds :: Core.Integer,
    -- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
    reward :: Types.Reward,
    -- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
    title :: Types.String,
    -- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
    description :: Types.String,
    -- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
    autoApprovalDelayInSeconds :: Core.Maybe Core.Integer,
    -- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
    keywords :: Core.Maybe Types.String,
    -- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Core.Maybe [Types.QualificationRequirement]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHITType' value with any optional fields omitted.
mkCreateHITType ::
  -- | 'assignmentDurationInSeconds'
  Core.Integer ->
  -- | 'reward'
  Types.Reward ->
  -- | 'title'
  Types.String ->
  -- | 'description'
  Types.String ->
  CreateHITType
mkCreateHITType
  assignmentDurationInSeconds
  reward
  title
  description =
    CreateHITType'
      { assignmentDurationInSeconds,
        reward,
        title,
        description,
        autoApprovalDelayInSeconds = Core.Nothing,
        keywords = Core.Nothing,
        qualificationRequirements = Core.Nothing
      }

-- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
--
-- /Note:/ Consider using 'assignmentDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittAssignmentDurationInSeconds :: Lens.Lens' CreateHITType Core.Integer
chittAssignmentDurationInSeconds = Lens.field @"assignmentDurationInSeconds"
{-# DEPRECATED chittAssignmentDurationInSeconds "Use generic-lens or generic-optics with 'assignmentDurationInSeconds' instead." #-}

-- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
--
-- /Note:/ Consider using 'reward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittReward :: Lens.Lens' CreateHITType Types.Reward
chittReward = Lens.field @"reward"
{-# DEPRECATED chittReward "Use generic-lens or generic-optics with 'reward' instead." #-}

-- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittTitle :: Lens.Lens' CreateHITType Types.String
chittTitle = Lens.field @"title"
{-# DEPRECATED chittTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittDescription :: Lens.Lens' CreateHITType Types.String
chittDescription = Lens.field @"description"
{-# DEPRECATED chittDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
--
-- /Note:/ Consider using 'autoApprovalDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittAutoApprovalDelayInSeconds :: Lens.Lens' CreateHITType (Core.Maybe Core.Integer)
chittAutoApprovalDelayInSeconds = Lens.field @"autoApprovalDelayInSeconds"
{-# DEPRECATED chittAutoApprovalDelayInSeconds "Use generic-lens or generic-optics with 'autoApprovalDelayInSeconds' instead." #-}

-- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittKeywords :: Lens.Lens' CreateHITType (Core.Maybe Types.String)
chittKeywords = Lens.field @"keywords"
{-# DEPRECATED chittKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- /Note:/ Consider using 'qualificationRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittQualificationRequirements :: Lens.Lens' CreateHITType (Core.Maybe [Types.QualificationRequirement])
chittQualificationRequirements = Lens.field @"qualificationRequirements"
{-# DEPRECATED chittQualificationRequirements "Use generic-lens or generic-optics with 'qualificationRequirements' instead." #-}

instance Core.FromJSON CreateHITType where
  toJSON CreateHITType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "AssignmentDurationInSeconds"
                  Core..= assignmentDurationInSeconds
              ),
            Core.Just ("Reward" Core..= reward),
            Core.Just ("Title" Core..= title),
            Core.Just ("Description" Core..= description),
            ("AutoApprovalDelayInSeconds" Core..=)
              Core.<$> autoApprovalDelayInSeconds,
            ("Keywords" Core..=) Core.<$> keywords,
            ("QualificationRequirements" Core..=)
              Core.<$> qualificationRequirements
          ]
      )

instance Core.AWSRequest CreateHITType where
  type Rs CreateHITType = CreateHITTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MTurkRequesterServiceV20170117.CreateHITType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHITTypeResponse'
            Core.<$> (x Core..:? "HITTypeId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateHITTypeResponse' smart constructor.
data CreateHITTypeResponse = CreateHITTypeResponse'
  { -- | The ID of the newly registered HIT type.
    hITTypeId :: Core.Maybe Types.HITTypeId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHITTypeResponse' value with any optional fields omitted.
mkCreateHITTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateHITTypeResponse
mkCreateHITTypeResponse responseStatus =
  CreateHITTypeResponse' {hITTypeId = Core.Nothing, responseStatus}

-- | The ID of the newly registered HIT type.
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittrrsHITTypeId :: Lens.Lens' CreateHITTypeResponse (Core.Maybe Types.HITTypeId)
chittrrsHITTypeId = Lens.field @"hITTypeId"
{-# DEPRECATED chittrrsHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittrrsResponseStatus :: Lens.Lens' CreateHITTypeResponse Core.Int
chittrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chittrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
