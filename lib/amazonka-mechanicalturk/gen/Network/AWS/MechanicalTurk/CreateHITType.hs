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
    chittAutoApprovalDelayInSeconds,
    chittReward,
    chittKeywords,
    chittQualificationRequirements,
    chittTitle,
    chittDescription,
    chittAssignmentDurationInSeconds,

    -- * Destructuring the response
    CreateHITTypeResponse (..),
    mkCreateHITTypeResponse,

    -- ** Response lenses
    chittrsHITTypeId,
    chittrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateHITType' smart constructor.
data CreateHITType = CreateHITType'
  { -- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
    autoApprovalDelayInSeconds :: Lude.Maybe Lude.Integer,
    -- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
    reward :: Lude.Text,
    -- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
    keywords :: Lude.Maybe Lude.Text,
    -- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Lude.Maybe [QualificationRequirement],
    -- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
    title :: Lude.Text,
    -- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
    description :: Lude.Text,
    -- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
    assignmentDurationInSeconds :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHITType' with the minimum fields required to make a request.
--
-- * 'autoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
-- * 'reward' - The amount of money the Requester will pay a Worker for successfully completing the HIT.
-- * 'keywords' - One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
-- * 'qualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
-- * 'title' - The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
-- * 'description' - A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
-- * 'assignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
mkCreateHITType ::
  -- | 'reward'
  Lude.Text ->
  -- | 'title'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'assignmentDurationInSeconds'
  Lude.Integer ->
  CreateHITType
mkCreateHITType
  pReward_
  pTitle_
  pDescription_
  pAssignmentDurationInSeconds_ =
    CreateHITType'
      { autoApprovalDelayInSeconds = Lude.Nothing,
        reward = pReward_,
        keywords = Lude.Nothing,
        qualificationRequirements = Lude.Nothing,
        title = pTitle_,
        description = pDescription_,
        assignmentDurationInSeconds = pAssignmentDurationInSeconds_
      }

-- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
--
-- /Note:/ Consider using 'autoApprovalDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittAutoApprovalDelayInSeconds :: Lens.Lens' CreateHITType (Lude.Maybe Lude.Integer)
chittAutoApprovalDelayInSeconds = Lens.lens (autoApprovalDelayInSeconds :: CreateHITType -> Lude.Maybe Lude.Integer) (\s a -> s {autoApprovalDelayInSeconds = a} :: CreateHITType)
{-# DEPRECATED chittAutoApprovalDelayInSeconds "Use generic-lens or generic-optics with 'autoApprovalDelayInSeconds' instead." #-}

-- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
--
-- /Note:/ Consider using 'reward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittReward :: Lens.Lens' CreateHITType Lude.Text
chittReward = Lens.lens (reward :: CreateHITType -> Lude.Text) (\s a -> s {reward = a} :: CreateHITType)
{-# DEPRECATED chittReward "Use generic-lens or generic-optics with 'reward' instead." #-}

-- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittKeywords :: Lens.Lens' CreateHITType (Lude.Maybe Lude.Text)
chittKeywords = Lens.lens (keywords :: CreateHITType -> Lude.Maybe Lude.Text) (\s a -> s {keywords = a} :: CreateHITType)
{-# DEPRECATED chittKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- /Note:/ Consider using 'qualificationRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittQualificationRequirements :: Lens.Lens' CreateHITType (Lude.Maybe [QualificationRequirement])
chittQualificationRequirements = Lens.lens (qualificationRequirements :: CreateHITType -> Lude.Maybe [QualificationRequirement]) (\s a -> s {qualificationRequirements = a} :: CreateHITType)
{-# DEPRECATED chittQualificationRequirements "Use generic-lens or generic-optics with 'qualificationRequirements' instead." #-}

-- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittTitle :: Lens.Lens' CreateHITType Lude.Text
chittTitle = Lens.lens (title :: CreateHITType -> Lude.Text) (\s a -> s {title = a} :: CreateHITType)
{-# DEPRECATED chittTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittDescription :: Lens.Lens' CreateHITType Lude.Text
chittDescription = Lens.lens (description :: CreateHITType -> Lude.Text) (\s a -> s {description = a} :: CreateHITType)
{-# DEPRECATED chittDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
--
-- /Note:/ Consider using 'assignmentDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittAssignmentDurationInSeconds :: Lens.Lens' CreateHITType Lude.Integer
chittAssignmentDurationInSeconds = Lens.lens (assignmentDurationInSeconds :: CreateHITType -> Lude.Integer) (\s a -> s {assignmentDurationInSeconds = a} :: CreateHITType)
{-# DEPRECATED chittAssignmentDurationInSeconds "Use generic-lens or generic-optics with 'assignmentDurationInSeconds' instead." #-}

instance Lude.AWSRequest CreateHITType where
  type Rs CreateHITType = CreateHITTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHITTypeResponse'
            Lude.<$> (x Lude..?> "HITTypeId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHITType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.CreateHITType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHITType where
  toJSON CreateHITType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoApprovalDelayInSeconds" Lude..=)
              Lude.<$> autoApprovalDelayInSeconds,
            Lude.Just ("Reward" Lude..= reward),
            ("Keywords" Lude..=) Lude.<$> keywords,
            ("QualificationRequirements" Lude..=)
              Lude.<$> qualificationRequirements,
            Lude.Just ("Title" Lude..= title),
            Lude.Just ("Description" Lude..= description),
            Lude.Just
              ( "AssignmentDurationInSeconds"
                  Lude..= assignmentDurationInSeconds
              )
          ]
      )

instance Lude.ToPath CreateHITType where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHITType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHITTypeResponse' smart constructor.
data CreateHITTypeResponse = CreateHITTypeResponse'
  { -- | The ID of the newly registered HIT type.
    hITTypeId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHITTypeResponse' with the minimum fields required to make a request.
--
-- * 'hITTypeId' - The ID of the newly registered HIT type.
-- * 'responseStatus' - The response status code.
mkCreateHITTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHITTypeResponse
mkCreateHITTypeResponse pResponseStatus_ =
  CreateHITTypeResponse'
    { hITTypeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the newly registered HIT type.
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittrsHITTypeId :: Lens.Lens' CreateHITTypeResponse (Lude.Maybe Lude.Text)
chittrsHITTypeId = Lens.lens (hITTypeId :: CreateHITTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {hITTypeId = a} :: CreateHITTypeResponse)
{-# DEPRECATED chittrsHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chittrsResponseStatus :: Lens.Lens' CreateHITTypeResponse Lude.Int
chittrsResponseStatus = Lens.lens (responseStatus :: CreateHITTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHITTypeResponse)
{-# DEPRECATED chittrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
