{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHIT@ operation creates a new Human Intelligence Task (HIT). The new HIT is made available for Workers to find and accept on the Amazon Mechanical Turk website.
--
-- This operation allows you to specify a new HIT by passing in values for the properties of the HIT, such as its title, reward amount and number of assignments. When you pass these values to @CreateHIT@ , a new HIT is created for you, with a new @HITTypeID@ . The HITTypeID can be used to create additional HITs in the future without needing to specify common parameters such as the title, description and reward amount each time.
-- An alternative way to create HITs is to first generate a HITTypeID using the @CreateHITType@ operation and then call the @CreateHITWithHITType@ operation. This is the recommended best practice for Requesters who are creating large numbers of HITs.
-- CreateHIT also supports several ways to provide question data: by providing a value for the @Question@ parameter that fully specifies the contents of the HIT, or by providing a @HitLayoutId@ and associated @HitLayoutParameters@ .
module Network.AWS.MechanicalTurk.CreateHIT
  ( -- * Creating a request
    CreateHIT (..),
    mkCreateHIT,

    -- ** Request lenses
    chitLifetimeInSeconds,
    chitHITReviewPolicy,
    chitUniqueRequestToken,
    chitAutoApprovalDelayInSeconds,
    chitRequesterAnnotation,
    chitMaxAssignments,
    chitReward,
    chitKeywords,
    chitHITLayoutId,
    chitHITLayoutParameters,
    chitQualificationRequirements,
    chitTitle,
    chitDescription,
    chitQuestion,
    chitAssignmentReviewPolicy,
    chitAssignmentDurationInSeconds,

    -- * Destructuring the response
    CreateHITResponse (..),
    mkCreateHITResponse,

    -- ** Response lenses
    chitrsHIT,
    chitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateHIT' smart constructor.
data CreateHIT = CreateHIT'
  { -- | An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted.
    lifetimeInSeconds :: Lude.Integer,
    -- | The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
    hITReviewPolicy :: Lude.Maybe ReviewPolicy,
    -- | A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId.
    uniqueRequestToken :: Lude.Maybe Lude.Text,
    -- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
    autoApprovalDelayInSeconds :: Lude.Maybe Lude.Integer,
    -- | An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.
    --
    -- The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.
    -- The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped.
    requesterAnnotation :: Lude.Maybe Lude.Text,
    -- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
    maxAssignments :: Lude.Maybe Lude.Int,
    -- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
    reward :: Lude.Text,
    -- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
    keywords :: Lude.Maybe Lude.Text,
    -- | The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.
    --
    -- Constraints: Either a Question parameter or a HITLayoutId parameter must be provided.
    hITLayoutId :: Lude.Maybe Lude.Text,
    -- | If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout.
    hITLayoutParameters :: Lude.Maybe [HITLayoutParameter],
    -- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Lude.Maybe [QualificationRequirement],
    -- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
    title :: Lude.Text,
    -- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
    description :: Lude.Text,
    -- | The data the person completing the HIT uses to produce the results.
    --
    -- Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.
    -- Either a Question parameter or a HITLayoutId parameter must be provided.
    question :: Lude.Maybe Lude.Text,
    -- | The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
    assignmentReviewPolicy :: Lude.Maybe ReviewPolicy,
    -- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
    assignmentDurationInSeconds :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHIT' with the minimum fields required to make a request.
--
-- * 'lifetimeInSeconds' - An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted.
-- * 'hITReviewPolicy' - The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
-- * 'uniqueRequestToken' - A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId.
-- * 'autoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
-- * 'requesterAnnotation' - An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.
--
-- The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.
-- The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped.
-- * 'maxAssignments' - The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
-- * 'reward' - The amount of money the Requester will pay a Worker for successfully completing the HIT.
-- * 'keywords' - One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
-- * 'hITLayoutId' - The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.
--
-- Constraints: Either a Question parameter or a HITLayoutId parameter must be provided.
-- * 'hITLayoutParameters' - If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout.
-- * 'qualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
-- * 'title' - The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
-- * 'description' - A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
-- * 'question' - The data the person completing the HIT uses to produce the results.
--
-- Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.
-- Either a Question parameter or a HITLayoutId parameter must be provided.
-- * 'assignmentReviewPolicy' - The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
-- * 'assignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
mkCreateHIT ::
  -- | 'lifetimeInSeconds'
  Lude.Integer ->
  -- | 'reward'
  Lude.Text ->
  -- | 'title'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'assignmentDurationInSeconds'
  Lude.Integer ->
  CreateHIT
mkCreateHIT
  pLifetimeInSeconds_
  pReward_
  pTitle_
  pDescription_
  pAssignmentDurationInSeconds_ =
    CreateHIT'
      { lifetimeInSeconds = pLifetimeInSeconds_,
        hITReviewPolicy = Lude.Nothing,
        uniqueRequestToken = Lude.Nothing,
        autoApprovalDelayInSeconds = Lude.Nothing,
        requesterAnnotation = Lude.Nothing,
        maxAssignments = Lude.Nothing,
        reward = pReward_,
        keywords = Lude.Nothing,
        hITLayoutId = Lude.Nothing,
        hITLayoutParameters = Lude.Nothing,
        qualificationRequirements = Lude.Nothing,
        title = pTitle_,
        description = pDescription_,
        question = Lude.Nothing,
        assignmentReviewPolicy = Lude.Nothing,
        assignmentDurationInSeconds = pAssignmentDurationInSeconds_
      }

-- | An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted.
--
-- /Note:/ Consider using 'lifetimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitLifetimeInSeconds :: Lens.Lens' CreateHIT Lude.Integer
chitLifetimeInSeconds = Lens.lens (lifetimeInSeconds :: CreateHIT -> Lude.Integer) (\s a -> s {lifetimeInSeconds = a} :: CreateHIT)
{-# DEPRECATED chitLifetimeInSeconds "Use generic-lens or generic-optics with 'lifetimeInSeconds' instead." #-}

-- | The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
--
-- /Note:/ Consider using 'hITReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitHITReviewPolicy :: Lens.Lens' CreateHIT (Lude.Maybe ReviewPolicy)
chitHITReviewPolicy = Lens.lens (hITReviewPolicy :: CreateHIT -> Lude.Maybe ReviewPolicy) (\s a -> s {hITReviewPolicy = a} :: CreateHIT)
{-# DEPRECATED chitHITReviewPolicy "Use generic-lens or generic-optics with 'hITReviewPolicy' instead." #-}

-- | A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId.
--
-- /Note:/ Consider using 'uniqueRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitUniqueRequestToken :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Text)
chitUniqueRequestToken = Lens.lens (uniqueRequestToken :: CreateHIT -> Lude.Maybe Lude.Text) (\s a -> s {uniqueRequestToken = a} :: CreateHIT)
{-# DEPRECATED chitUniqueRequestToken "Use generic-lens or generic-optics with 'uniqueRequestToken' instead." #-}

-- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
--
-- /Note:/ Consider using 'autoApprovalDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitAutoApprovalDelayInSeconds :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Integer)
chitAutoApprovalDelayInSeconds = Lens.lens (autoApprovalDelayInSeconds :: CreateHIT -> Lude.Maybe Lude.Integer) (\s a -> s {autoApprovalDelayInSeconds = a} :: CreateHIT)
{-# DEPRECATED chitAutoApprovalDelayInSeconds "Use generic-lens or generic-optics with 'autoApprovalDelayInSeconds' instead." #-}

-- | An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.
--
-- The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.
-- The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped.
--
-- /Note:/ Consider using 'requesterAnnotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitRequesterAnnotation :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Text)
chitRequesterAnnotation = Lens.lens (requesterAnnotation :: CreateHIT -> Lude.Maybe Lude.Text) (\s a -> s {requesterAnnotation = a} :: CreateHIT)
{-# DEPRECATED chitRequesterAnnotation "Use generic-lens or generic-optics with 'requesterAnnotation' instead." #-}

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
--
-- /Note:/ Consider using 'maxAssignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitMaxAssignments :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Int)
chitMaxAssignments = Lens.lens (maxAssignments :: CreateHIT -> Lude.Maybe Lude.Int) (\s a -> s {maxAssignments = a} :: CreateHIT)
{-# DEPRECATED chitMaxAssignments "Use generic-lens or generic-optics with 'maxAssignments' instead." #-}

-- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
--
-- /Note:/ Consider using 'reward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitReward :: Lens.Lens' CreateHIT Lude.Text
chitReward = Lens.lens (reward :: CreateHIT -> Lude.Text) (\s a -> s {reward = a} :: CreateHIT)
{-# DEPRECATED chitReward "Use generic-lens or generic-optics with 'reward' instead." #-}

-- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitKeywords :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Text)
chitKeywords = Lens.lens (keywords :: CreateHIT -> Lude.Maybe Lude.Text) (\s a -> s {keywords = a} :: CreateHIT)
{-# DEPRECATED chitKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.
--
-- Constraints: Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- /Note:/ Consider using 'hITLayoutId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitHITLayoutId :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Text)
chitHITLayoutId = Lens.lens (hITLayoutId :: CreateHIT -> Lude.Maybe Lude.Text) (\s a -> s {hITLayoutId = a} :: CreateHIT)
{-# DEPRECATED chitHITLayoutId "Use generic-lens or generic-optics with 'hITLayoutId' instead." #-}

-- | If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout.
--
-- /Note:/ Consider using 'hITLayoutParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitHITLayoutParameters :: Lens.Lens' CreateHIT (Lude.Maybe [HITLayoutParameter])
chitHITLayoutParameters = Lens.lens (hITLayoutParameters :: CreateHIT -> Lude.Maybe [HITLayoutParameter]) (\s a -> s {hITLayoutParameters = a} :: CreateHIT)
{-# DEPRECATED chitHITLayoutParameters "Use generic-lens or generic-optics with 'hITLayoutParameters' instead." #-}

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- /Note:/ Consider using 'qualificationRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitQualificationRequirements :: Lens.Lens' CreateHIT (Lude.Maybe [QualificationRequirement])
chitQualificationRequirements = Lens.lens (qualificationRequirements :: CreateHIT -> Lude.Maybe [QualificationRequirement]) (\s a -> s {qualificationRequirements = a} :: CreateHIT)
{-# DEPRECATED chitQualificationRequirements "Use generic-lens or generic-optics with 'qualificationRequirements' instead." #-}

-- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitTitle :: Lens.Lens' CreateHIT Lude.Text
chitTitle = Lens.lens (title :: CreateHIT -> Lude.Text) (\s a -> s {title = a} :: CreateHIT)
{-# DEPRECATED chitTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitDescription :: Lens.Lens' CreateHIT Lude.Text
chitDescription = Lens.lens (description :: CreateHIT -> Lude.Text) (\s a -> s {description = a} :: CreateHIT)
{-# DEPRECATED chitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The data the person completing the HIT uses to produce the results.
--
-- Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.
-- Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- /Note:/ Consider using 'question' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitQuestion :: Lens.Lens' CreateHIT (Lude.Maybe Lude.Text)
chitQuestion = Lens.lens (question :: CreateHIT -> Lude.Maybe Lude.Text) (\s a -> s {question = a} :: CreateHIT)
{-# DEPRECATED chitQuestion "Use generic-lens or generic-optics with 'question' instead." #-}

-- | The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
--
-- /Note:/ Consider using 'assignmentReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitAssignmentReviewPolicy :: Lens.Lens' CreateHIT (Lude.Maybe ReviewPolicy)
chitAssignmentReviewPolicy = Lens.lens (assignmentReviewPolicy :: CreateHIT -> Lude.Maybe ReviewPolicy) (\s a -> s {assignmentReviewPolicy = a} :: CreateHIT)
{-# DEPRECATED chitAssignmentReviewPolicy "Use generic-lens or generic-optics with 'assignmentReviewPolicy' instead." #-}

-- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
--
-- /Note:/ Consider using 'assignmentDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitAssignmentDurationInSeconds :: Lens.Lens' CreateHIT Lude.Integer
chitAssignmentDurationInSeconds = Lens.lens (assignmentDurationInSeconds :: CreateHIT -> Lude.Integer) (\s a -> s {assignmentDurationInSeconds = a} :: CreateHIT)
{-# DEPRECATED chitAssignmentDurationInSeconds "Use generic-lens or generic-optics with 'assignmentDurationInSeconds' instead." #-}

instance Lude.AWSRequest CreateHIT where
  type Rs CreateHIT = CreateHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHITResponse'
            Lude.<$> (x Lude..?> "HIT") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MTurkRequesterServiceV20170117.CreateHIT" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHIT where
  toJSON CreateHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LifetimeInSeconds" Lude..= lifetimeInSeconds),
            ("HITReviewPolicy" Lude..=) Lude.<$> hITReviewPolicy,
            ("UniqueRequestToken" Lude..=) Lude.<$> uniqueRequestToken,
            ("AutoApprovalDelayInSeconds" Lude..=)
              Lude.<$> autoApprovalDelayInSeconds,
            ("RequesterAnnotation" Lude..=) Lude.<$> requesterAnnotation,
            ("MaxAssignments" Lude..=) Lude.<$> maxAssignments,
            Lude.Just ("Reward" Lude..= reward),
            ("Keywords" Lude..=) Lude.<$> keywords,
            ("HITLayoutId" Lude..=) Lude.<$> hITLayoutId,
            ("HITLayoutParameters" Lude..=) Lude.<$> hITLayoutParameters,
            ("QualificationRequirements" Lude..=)
              Lude.<$> qualificationRequirements,
            Lude.Just ("Title" Lude..= title),
            Lude.Just ("Description" Lude..= description),
            ("Question" Lude..=) Lude.<$> question,
            ("AssignmentReviewPolicy" Lude..=) Lude.<$> assignmentReviewPolicy,
            Lude.Just
              ( "AssignmentDurationInSeconds"
                  Lude..= assignmentDurationInSeconds
              )
          ]
      )

instance Lude.ToPath CreateHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHITResponse' smart constructor.
data CreateHITResponse = CreateHITResponse'
  { -- | Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation.
    hIT :: Lude.Maybe HIT,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHITResponse' with the minimum fields required to make a request.
--
-- * 'hIT' - Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation.
-- * 'responseStatus' - The response status code.
mkCreateHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHITResponse
mkCreateHITResponse pResponseStatus_ =
  CreateHITResponse'
    { hIT = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation.
--
-- /Note:/ Consider using 'hIT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitrsHIT :: Lens.Lens' CreateHITResponse (Lude.Maybe HIT)
chitrsHIT = Lens.lens (hIT :: CreateHITResponse -> Lude.Maybe HIT) (\s a -> s {hIT = a} :: CreateHITResponse)
{-# DEPRECATED chitrsHIT "Use generic-lens or generic-optics with 'hIT' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chitrsResponseStatus :: Lens.Lens' CreateHITResponse Lude.Int
chitrsResponseStatus = Lens.lens (responseStatus :: CreateHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHITResponse)
{-# DEPRECATED chitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
