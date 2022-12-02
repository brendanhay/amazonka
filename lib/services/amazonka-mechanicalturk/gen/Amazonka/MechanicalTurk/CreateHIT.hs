{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MechanicalTurk.CreateHIT
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHIT@ operation creates a new Human Intelligence Task (HIT).
-- The new HIT is made available for Workers to find and accept on the
-- Amazon Mechanical Turk website.
--
-- This operation allows you to specify a new HIT by passing in values for
-- the properties of the HIT, such as its title, reward amount and number
-- of assignments. When you pass these values to @CreateHIT@, a new HIT is
-- created for you, with a new @HITTypeID@. The HITTypeID can be used to
-- create additional HITs in the future without needing to specify common
-- parameters such as the title, description and reward amount each time.
--
-- An alternative way to create HITs is to first generate a HITTypeID using
-- the @CreateHITType@ operation and then call the @CreateHITWithHITType@
-- operation. This is the recommended best practice for Requesters who are
-- creating large numbers of HITs.
--
-- CreateHIT also supports several ways to provide question data: by
-- providing a value for the @Question@ parameter that fully specifies the
-- contents of the HIT, or by providing a @HitLayoutId@ and associated
-- @HitLayoutParameters@.
--
-- If a HIT is created with 10 or more maximum assignments, there is an
-- additional fee. For more information, see
-- <https://requester.mturk.com/pricing Amazon Mechanical Turk Pricing>.
module Amazonka.MechanicalTurk.CreateHIT
  ( -- * Creating a Request
    CreateHIT (..),
    newCreateHIT,

    -- * Request Lenses
    createHIT_hITLayoutParameters,
    createHIT_hITReviewPolicy,
    createHIT_maxAssignments,
    createHIT_keywords,
    createHIT_question,
    createHIT_autoApprovalDelayInSeconds,
    createHIT_qualificationRequirements,
    createHIT_uniqueRequestToken,
    createHIT_hITLayoutId,
    createHIT_assignmentReviewPolicy,
    createHIT_requesterAnnotation,
    createHIT_lifetimeInSeconds,
    createHIT_assignmentDurationInSeconds,
    createHIT_reward,
    createHIT_title,
    createHIT_description,

    -- * Destructuring the Response
    CreateHITResponse (..),
    newCreateHITResponse,

    -- * Response Lenses
    createHITResponse_hit,
    createHITResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateHIT' smart constructor.
data CreateHIT = CreateHIT'
  { -- | If the HITLayoutId is provided, any placeholder values must be filled in
    -- with values using the HITLayoutParameter structure. For more
    -- information, see HITLayout.
    hITLayoutParameters :: Prelude.Maybe [HITLayoutParameter],
    -- | The HIT-level Review Policy applies to the HIT. You can specify for
    -- Mechanical Turk to take various actions based on the policy.
    hITReviewPolicy :: Prelude.Maybe ReviewPolicy,
    -- | The number of times the HIT can be accepted and completed before the HIT
    -- becomes unavailable.
    maxAssignments :: Prelude.Maybe Prelude.Int,
    -- | One or more words or phrases that describe the HIT, separated by commas.
    -- These words are used in searches to find HITs.
    keywords :: Prelude.Maybe Prelude.Text,
    -- | The data the person completing the HIT uses to produce the results.
    --
    -- Constraints: Must be a QuestionForm data structure, an ExternalQuestion
    -- data structure, or an HTMLQuestion data structure. The XML question data
    -- must not be larger than 64 kilobytes (65,535 bytes) in size, including
    -- whitespace.
    --
    -- Either a Question parameter or a HITLayoutId parameter must be provided.
    question :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds after an assignment for the HIT has been
    -- submitted, after which the assignment is considered Approved
    -- automatically unless the Requester explicitly rejects it.
    autoApprovalDelayInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | Conditions that a Worker\'s Qualifications must meet in order to accept
    -- the HIT. A HIT can have between zero and ten Qualification requirements.
    -- All requirements must be met in order for a Worker to accept the HIT.
    -- Additionally, other actions can be restricted using the @ActionsGuarded@
    -- field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Prelude.Maybe [QualificationRequirement],
    -- | A unique identifier for this request which allows you to retry the call
    -- on error without creating duplicate HITs. This is useful in cases such
    -- as network timeouts where it is unclear whether or not the call
    -- succeeded on the server. If the HIT already exists in the system from a
    -- previous call using the same UniqueRequestToken, subsequent calls will
    -- return a AWS.MechanicalTurk.HitAlreadyExists error with a message
    -- containing the HITId.
    --
    -- Note: It is your responsibility to ensure uniqueness of the token. The
    -- unique token expires after 24 hours. Subsequent calls using the same
    -- UniqueRequestToken made after the 24 hour limit could create duplicate
    -- HITs.
    uniqueRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The HITLayoutId allows you to use a pre-existing HIT design with
    -- placeholder values and create an additional HIT by providing those
    -- values as HITLayoutParameters.
    --
    -- Constraints: Either a Question parameter or a HITLayoutId parameter must
    -- be provided.
    hITLayoutId :: Prelude.Maybe Prelude.Text,
    -- | The Assignment-level Review Policy applies to the assignments under the
    -- HIT. You can specify for Mechanical Turk to take various actions based
    -- on the policy.
    assignmentReviewPolicy :: Prelude.Maybe ReviewPolicy,
    -- | An arbitrary data field. The RequesterAnnotation parameter lets your
    -- application attach arbitrary data to the HIT for tracking purposes. For
    -- example, this parameter could be an identifier internal to the
    -- Requester\'s application that corresponds with the HIT.
    --
    -- The RequesterAnnotation parameter for a HIT is only visible to the
    -- Requester who created the HIT. It is not shown to the Worker, or any
    -- other Requester.
    --
    -- The RequesterAnnotation parameter may be different for each HIT you
    -- submit. It does not affect how your HITs are grouped.
    requesterAnnotation :: Prelude.Maybe Prelude.Text,
    -- | An amount of time, in seconds, after which the HIT is no longer
    -- available for users to accept. After the lifetime of the HIT elapses,
    -- the HIT no longer appears in HIT searches, even if not all of the
    -- assignments for the HIT have been accepted.
    lifetimeInSeconds :: Prelude.Integer,
    -- | The amount of time, in seconds, that a Worker has to complete the HIT
    -- after accepting it. If a Worker does not complete the assignment within
    -- the specified duration, the assignment is considered abandoned. If the
    -- HIT is still active (that is, its lifetime has not elapsed), the
    -- assignment becomes available for other users to find and accept.
    assignmentDurationInSeconds :: Prelude.Integer,
    -- | The amount of money the Requester will pay a Worker for successfully
    -- completing the HIT.
    reward :: Prelude.Text,
    -- | The title of the HIT. A title should be short and descriptive about the
    -- kind of task the HIT contains. On the Amazon Mechanical Turk web site,
    -- the HIT title appears in search results, and everywhere the HIT is
    -- mentioned.
    title :: Prelude.Text,
    -- | A general description of the HIT. A description includes detailed
    -- information about the kind of task the HIT contains. On the Amazon
    -- Mechanical Turk web site, the HIT description appears in the expanded
    -- view of search results, and in the HIT and assignment screens. A good
    -- description gives the user enough information to evaluate the HIT before
    -- accepting it.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITLayoutParameters', 'createHIT_hITLayoutParameters' - If the HITLayoutId is provided, any placeholder values must be filled in
-- with values using the HITLayoutParameter structure. For more
-- information, see HITLayout.
--
-- 'hITReviewPolicy', 'createHIT_hITReviewPolicy' - The HIT-level Review Policy applies to the HIT. You can specify for
-- Mechanical Turk to take various actions based on the policy.
--
-- 'maxAssignments', 'createHIT_maxAssignments' - The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
--
-- 'keywords', 'createHIT_keywords' - One or more words or phrases that describe the HIT, separated by commas.
-- These words are used in searches to find HITs.
--
-- 'question', 'createHIT_question' - The data the person completing the HIT uses to produce the results.
--
-- Constraints: Must be a QuestionForm data structure, an ExternalQuestion
-- data structure, or an HTMLQuestion data structure. The XML question data
-- must not be larger than 64 kilobytes (65,535 bytes) in size, including
-- whitespace.
--
-- Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- 'autoApprovalDelayInSeconds', 'createHIT_autoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been
-- submitted, after which the assignment is considered Approved
-- automatically unless the Requester explicitly rejects it.
--
-- 'qualificationRequirements', 'createHIT_qualificationRequirements' - Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
--
-- 'uniqueRequestToken', 'createHIT_uniqueRequestToken' - A unique identifier for this request which allows you to retry the call
-- on error without creating duplicate HITs. This is useful in cases such
-- as network timeouts where it is unclear whether or not the call
-- succeeded on the server. If the HIT already exists in the system from a
-- previous call using the same UniqueRequestToken, subsequent calls will
-- return a AWS.MechanicalTurk.HitAlreadyExists error with a message
-- containing the HITId.
--
-- Note: It is your responsibility to ensure uniqueness of the token. The
-- unique token expires after 24 hours. Subsequent calls using the same
-- UniqueRequestToken made after the 24 hour limit could create duplicate
-- HITs.
--
-- 'hITLayoutId', 'createHIT_hITLayoutId' - The HITLayoutId allows you to use a pre-existing HIT design with
-- placeholder values and create an additional HIT by providing those
-- values as HITLayoutParameters.
--
-- Constraints: Either a Question parameter or a HITLayoutId parameter must
-- be provided.
--
-- 'assignmentReviewPolicy', 'createHIT_assignmentReviewPolicy' - The Assignment-level Review Policy applies to the assignments under the
-- HIT. You can specify for Mechanical Turk to take various actions based
-- on the policy.
--
-- 'requesterAnnotation', 'createHIT_requesterAnnotation' - An arbitrary data field. The RequesterAnnotation parameter lets your
-- application attach arbitrary data to the HIT for tracking purposes. For
-- example, this parameter could be an identifier internal to the
-- Requester\'s application that corresponds with the HIT.
--
-- The RequesterAnnotation parameter for a HIT is only visible to the
-- Requester who created the HIT. It is not shown to the Worker, or any
-- other Requester.
--
-- The RequesterAnnotation parameter may be different for each HIT you
-- submit. It does not affect how your HITs are grouped.
--
-- 'lifetimeInSeconds', 'createHIT_lifetimeInSeconds' - An amount of time, in seconds, after which the HIT is no longer
-- available for users to accept. After the lifetime of the HIT elapses,
-- the HIT no longer appears in HIT searches, even if not all of the
-- assignments for the HIT have been accepted.
--
-- 'assignmentDurationInSeconds', 'createHIT_assignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT
-- after accepting it. If a Worker does not complete the assignment within
-- the specified duration, the assignment is considered abandoned. If the
-- HIT is still active (that is, its lifetime has not elapsed), the
-- assignment becomes available for other users to find and accept.
--
-- 'reward', 'createHIT_reward' - The amount of money the Requester will pay a Worker for successfully
-- completing the HIT.
--
-- 'title', 'createHIT_title' - The title of the HIT. A title should be short and descriptive about the
-- kind of task the HIT contains. On the Amazon Mechanical Turk web site,
-- the HIT title appears in search results, and everywhere the HIT is
-- mentioned.
--
-- 'description', 'createHIT_description' - A general description of the HIT. A description includes detailed
-- information about the kind of task the HIT contains. On the Amazon
-- Mechanical Turk web site, the HIT description appears in the expanded
-- view of search results, and in the HIT and assignment screens. A good
-- description gives the user enough information to evaluate the HIT before
-- accepting it.
newCreateHIT ::
  -- | 'lifetimeInSeconds'
  Prelude.Integer ->
  -- | 'assignmentDurationInSeconds'
  Prelude.Integer ->
  -- | 'reward'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateHIT
newCreateHIT
  pLifetimeInSeconds_
  pAssignmentDurationInSeconds_
  pReward_
  pTitle_
  pDescription_ =
    CreateHIT'
      { hITLayoutParameters = Prelude.Nothing,
        hITReviewPolicy = Prelude.Nothing,
        maxAssignments = Prelude.Nothing,
        keywords = Prelude.Nothing,
        question = Prelude.Nothing,
        autoApprovalDelayInSeconds = Prelude.Nothing,
        qualificationRequirements = Prelude.Nothing,
        uniqueRequestToken = Prelude.Nothing,
        hITLayoutId = Prelude.Nothing,
        assignmentReviewPolicy = Prelude.Nothing,
        requesterAnnotation = Prelude.Nothing,
        lifetimeInSeconds = pLifetimeInSeconds_,
        assignmentDurationInSeconds =
          pAssignmentDurationInSeconds_,
        reward = pReward_,
        title = pTitle_,
        description = pDescription_
      }

-- | If the HITLayoutId is provided, any placeholder values must be filled in
-- with values using the HITLayoutParameter structure. For more
-- information, see HITLayout.
createHIT_hITLayoutParameters :: Lens.Lens' CreateHIT (Prelude.Maybe [HITLayoutParameter])
createHIT_hITLayoutParameters = Lens.lens (\CreateHIT' {hITLayoutParameters} -> hITLayoutParameters) (\s@CreateHIT' {} a -> s {hITLayoutParameters = a} :: CreateHIT) Prelude.. Lens.mapping Lens.coerced

-- | The HIT-level Review Policy applies to the HIT. You can specify for
-- Mechanical Turk to take various actions based on the policy.
createHIT_hITReviewPolicy :: Lens.Lens' CreateHIT (Prelude.Maybe ReviewPolicy)
createHIT_hITReviewPolicy = Lens.lens (\CreateHIT' {hITReviewPolicy} -> hITReviewPolicy) (\s@CreateHIT' {} a -> s {hITReviewPolicy = a} :: CreateHIT)

-- | The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
createHIT_maxAssignments :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Int)
createHIT_maxAssignments = Lens.lens (\CreateHIT' {maxAssignments} -> maxAssignments) (\s@CreateHIT' {} a -> s {maxAssignments = a} :: CreateHIT)

-- | One or more words or phrases that describe the HIT, separated by commas.
-- These words are used in searches to find HITs.
createHIT_keywords :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Text)
createHIT_keywords = Lens.lens (\CreateHIT' {keywords} -> keywords) (\s@CreateHIT' {} a -> s {keywords = a} :: CreateHIT)

-- | The data the person completing the HIT uses to produce the results.
--
-- Constraints: Must be a QuestionForm data structure, an ExternalQuestion
-- data structure, or an HTMLQuestion data structure. The XML question data
-- must not be larger than 64 kilobytes (65,535 bytes) in size, including
-- whitespace.
--
-- Either a Question parameter or a HITLayoutId parameter must be provided.
createHIT_question :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Text)
createHIT_question = Lens.lens (\CreateHIT' {question} -> question) (\s@CreateHIT' {} a -> s {question = a} :: CreateHIT)

-- | The number of seconds after an assignment for the HIT has been
-- submitted, after which the assignment is considered Approved
-- automatically unless the Requester explicitly rejects it.
createHIT_autoApprovalDelayInSeconds :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Integer)
createHIT_autoApprovalDelayInSeconds = Lens.lens (\CreateHIT' {autoApprovalDelayInSeconds} -> autoApprovalDelayInSeconds) (\s@CreateHIT' {} a -> s {autoApprovalDelayInSeconds = a} :: CreateHIT)

-- | Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
createHIT_qualificationRequirements :: Lens.Lens' CreateHIT (Prelude.Maybe [QualificationRequirement])
createHIT_qualificationRequirements = Lens.lens (\CreateHIT' {qualificationRequirements} -> qualificationRequirements) (\s@CreateHIT' {} a -> s {qualificationRequirements = a} :: CreateHIT) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for this request which allows you to retry the call
-- on error without creating duplicate HITs. This is useful in cases such
-- as network timeouts where it is unclear whether or not the call
-- succeeded on the server. If the HIT already exists in the system from a
-- previous call using the same UniqueRequestToken, subsequent calls will
-- return a AWS.MechanicalTurk.HitAlreadyExists error with a message
-- containing the HITId.
--
-- Note: It is your responsibility to ensure uniqueness of the token. The
-- unique token expires after 24 hours. Subsequent calls using the same
-- UniqueRequestToken made after the 24 hour limit could create duplicate
-- HITs.
createHIT_uniqueRequestToken :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Text)
createHIT_uniqueRequestToken = Lens.lens (\CreateHIT' {uniqueRequestToken} -> uniqueRequestToken) (\s@CreateHIT' {} a -> s {uniqueRequestToken = a} :: CreateHIT)

-- | The HITLayoutId allows you to use a pre-existing HIT design with
-- placeholder values and create an additional HIT by providing those
-- values as HITLayoutParameters.
--
-- Constraints: Either a Question parameter or a HITLayoutId parameter must
-- be provided.
createHIT_hITLayoutId :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Text)
createHIT_hITLayoutId = Lens.lens (\CreateHIT' {hITLayoutId} -> hITLayoutId) (\s@CreateHIT' {} a -> s {hITLayoutId = a} :: CreateHIT)

-- | The Assignment-level Review Policy applies to the assignments under the
-- HIT. You can specify for Mechanical Turk to take various actions based
-- on the policy.
createHIT_assignmentReviewPolicy :: Lens.Lens' CreateHIT (Prelude.Maybe ReviewPolicy)
createHIT_assignmentReviewPolicy = Lens.lens (\CreateHIT' {assignmentReviewPolicy} -> assignmentReviewPolicy) (\s@CreateHIT' {} a -> s {assignmentReviewPolicy = a} :: CreateHIT)

-- | An arbitrary data field. The RequesterAnnotation parameter lets your
-- application attach arbitrary data to the HIT for tracking purposes. For
-- example, this parameter could be an identifier internal to the
-- Requester\'s application that corresponds with the HIT.
--
-- The RequesterAnnotation parameter for a HIT is only visible to the
-- Requester who created the HIT. It is not shown to the Worker, or any
-- other Requester.
--
-- The RequesterAnnotation parameter may be different for each HIT you
-- submit. It does not affect how your HITs are grouped.
createHIT_requesterAnnotation :: Lens.Lens' CreateHIT (Prelude.Maybe Prelude.Text)
createHIT_requesterAnnotation = Lens.lens (\CreateHIT' {requesterAnnotation} -> requesterAnnotation) (\s@CreateHIT' {} a -> s {requesterAnnotation = a} :: CreateHIT)

-- | An amount of time, in seconds, after which the HIT is no longer
-- available for users to accept. After the lifetime of the HIT elapses,
-- the HIT no longer appears in HIT searches, even if not all of the
-- assignments for the HIT have been accepted.
createHIT_lifetimeInSeconds :: Lens.Lens' CreateHIT Prelude.Integer
createHIT_lifetimeInSeconds = Lens.lens (\CreateHIT' {lifetimeInSeconds} -> lifetimeInSeconds) (\s@CreateHIT' {} a -> s {lifetimeInSeconds = a} :: CreateHIT)

-- | The amount of time, in seconds, that a Worker has to complete the HIT
-- after accepting it. If a Worker does not complete the assignment within
-- the specified duration, the assignment is considered abandoned. If the
-- HIT is still active (that is, its lifetime has not elapsed), the
-- assignment becomes available for other users to find and accept.
createHIT_assignmentDurationInSeconds :: Lens.Lens' CreateHIT Prelude.Integer
createHIT_assignmentDurationInSeconds = Lens.lens (\CreateHIT' {assignmentDurationInSeconds} -> assignmentDurationInSeconds) (\s@CreateHIT' {} a -> s {assignmentDurationInSeconds = a} :: CreateHIT)

-- | The amount of money the Requester will pay a Worker for successfully
-- completing the HIT.
createHIT_reward :: Lens.Lens' CreateHIT Prelude.Text
createHIT_reward = Lens.lens (\CreateHIT' {reward} -> reward) (\s@CreateHIT' {} a -> s {reward = a} :: CreateHIT)

-- | The title of the HIT. A title should be short and descriptive about the
-- kind of task the HIT contains. On the Amazon Mechanical Turk web site,
-- the HIT title appears in search results, and everywhere the HIT is
-- mentioned.
createHIT_title :: Lens.Lens' CreateHIT Prelude.Text
createHIT_title = Lens.lens (\CreateHIT' {title} -> title) (\s@CreateHIT' {} a -> s {title = a} :: CreateHIT)

-- | A general description of the HIT. A description includes detailed
-- information about the kind of task the HIT contains. On the Amazon
-- Mechanical Turk web site, the HIT description appears in the expanded
-- view of search results, and in the HIT and assignment screens. A good
-- description gives the user enough information to evaluate the HIT before
-- accepting it.
createHIT_description :: Lens.Lens' CreateHIT Prelude.Text
createHIT_description = Lens.lens (\CreateHIT' {description} -> description) (\s@CreateHIT' {} a -> s {description = a} :: CreateHIT)

instance Core.AWSRequest CreateHIT where
  type AWSResponse CreateHIT = CreateHITResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHITResponse'
            Prelude.<$> (x Data..?> "HIT")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHIT where
  hashWithSalt _salt CreateHIT' {..} =
    _salt `Prelude.hashWithSalt` hITLayoutParameters
      `Prelude.hashWithSalt` hITReviewPolicy
      `Prelude.hashWithSalt` maxAssignments
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` question
      `Prelude.hashWithSalt` autoApprovalDelayInSeconds
      `Prelude.hashWithSalt` qualificationRequirements
      `Prelude.hashWithSalt` uniqueRequestToken
      `Prelude.hashWithSalt` hITLayoutId
      `Prelude.hashWithSalt` assignmentReviewPolicy
      `Prelude.hashWithSalt` requesterAnnotation
      `Prelude.hashWithSalt` lifetimeInSeconds
      `Prelude.hashWithSalt` assignmentDurationInSeconds
      `Prelude.hashWithSalt` reward
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateHIT where
  rnf CreateHIT' {..} =
    Prelude.rnf hITLayoutParameters
      `Prelude.seq` Prelude.rnf hITReviewPolicy
      `Prelude.seq` Prelude.rnf maxAssignments
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf question
      `Prelude.seq` Prelude.rnf autoApprovalDelayInSeconds
      `Prelude.seq` Prelude.rnf qualificationRequirements
      `Prelude.seq` Prelude.rnf uniqueRequestToken
      `Prelude.seq` Prelude.rnf hITLayoutId
      `Prelude.seq` Prelude.rnf assignmentReviewPolicy
      `Prelude.seq` Prelude.rnf requesterAnnotation
      `Prelude.seq` Prelude.rnf lifetimeInSeconds
      `Prelude.seq` Prelude.rnf assignmentDurationInSeconds
      `Prelude.seq` Prelude.rnf reward
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.CreateHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHIT where
  toJSON CreateHIT' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HITLayoutParameters" Data..=)
              Prelude.<$> hITLayoutParameters,
            ("HITReviewPolicy" Data..=)
              Prelude.<$> hITReviewPolicy,
            ("MaxAssignments" Data..=)
              Prelude.<$> maxAssignments,
            ("Keywords" Data..=) Prelude.<$> keywords,
            ("Question" Data..=) Prelude.<$> question,
            ("AutoApprovalDelayInSeconds" Data..=)
              Prelude.<$> autoApprovalDelayInSeconds,
            ("QualificationRequirements" Data..=)
              Prelude.<$> qualificationRequirements,
            ("UniqueRequestToken" Data..=)
              Prelude.<$> uniqueRequestToken,
            ("HITLayoutId" Data..=) Prelude.<$> hITLayoutId,
            ("AssignmentReviewPolicy" Data..=)
              Prelude.<$> assignmentReviewPolicy,
            ("RequesterAnnotation" Data..=)
              Prelude.<$> requesterAnnotation,
            Prelude.Just
              ("LifetimeInSeconds" Data..= lifetimeInSeconds),
            Prelude.Just
              ( "AssignmentDurationInSeconds"
                  Data..= assignmentDurationInSeconds
              ),
            Prelude.Just ("Reward" Data..= reward),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("Description" Data..= description)
          ]
      )

instance Data.ToPath CreateHIT where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHITResponse' smart constructor.
data CreateHITResponse = CreateHITResponse'
  { -- | Contains the newly created HIT data. For a description of the HIT data
    -- structure as it appears in responses, see the HIT Data Structure
    -- documentation.
    hit :: Prelude.Maybe HIT,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hit', 'createHITResponse_hit' - Contains the newly created HIT data. For a description of the HIT data
-- structure as it appears in responses, see the HIT Data Structure
-- documentation.
--
-- 'httpStatus', 'createHITResponse_httpStatus' - The response's http status code.
newCreateHITResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHITResponse
newCreateHITResponse pHttpStatus_ =
  CreateHITResponse'
    { hit = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the newly created HIT data. For a description of the HIT data
-- structure as it appears in responses, see the HIT Data Structure
-- documentation.
createHITResponse_hit :: Lens.Lens' CreateHITResponse (Prelude.Maybe HIT)
createHITResponse_hit = Lens.lens (\CreateHITResponse' {hit} -> hit) (\s@CreateHITResponse' {} a -> s {hit = a} :: CreateHITResponse)

-- | The response's http status code.
createHITResponse_httpStatus :: Lens.Lens' CreateHITResponse Prelude.Int
createHITResponse_httpStatus = Lens.lens (\CreateHITResponse' {httpStatus} -> httpStatus) (\s@CreateHITResponse' {} a -> s {httpStatus = a} :: CreateHITResponse)

instance Prelude.NFData CreateHITResponse where
  rnf CreateHITResponse' {..} =
    Prelude.rnf hit
      `Prelude.seq` Prelude.rnf httpStatus
