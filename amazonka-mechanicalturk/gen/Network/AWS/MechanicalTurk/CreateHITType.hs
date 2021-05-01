{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MechanicalTurk.CreateHITType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHITType@ operation creates a new HIT type. This operation
-- allows you to define a standard set of HIT properties to use when
-- creating HITs. If you register a HIT type with values that match an
-- existing HIT type, the HIT type ID of the existing type will be
-- returned.
module Network.AWS.MechanicalTurk.CreateHITType
  ( -- * Creating a Request
    CreateHITType (..),
    newCreateHITType,

    -- * Request Lenses
    createHITType_autoApprovalDelayInSeconds,
    createHITType_qualificationRequirements,
    createHITType_keywords,
    createHITType_assignmentDurationInSeconds,
    createHITType_reward,
    createHITType_title,
    createHITType_description,

    -- * Destructuring the Response
    CreateHITTypeResponse (..),
    newCreateHITTypeResponse,

    -- * Response Lenses
    createHITTypeResponse_hITTypeId,
    createHITTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateHITType' smart constructor.
data CreateHITType = CreateHITType'
  { -- | The number of seconds after an assignment for the HIT has been
    -- submitted, after which the assignment is considered Approved
    -- automatically unless the Requester explicitly rejects it.
    autoApprovalDelayInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | Conditions that a Worker\'s Qualifications must meet in order to accept
    -- the HIT. A HIT can have between zero and ten Qualification requirements.
    -- All requirements must be met in order for a Worker to accept the HIT.
    -- Additionally, other actions can be restricted using the @ActionsGuarded@
    -- field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Prelude.Maybe [QualificationRequirement],
    -- | One or more words or phrases that describe the HIT, separated by commas.
    -- These words are used in searches to find HITs.
    keywords :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateHITType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoApprovalDelayInSeconds', 'createHITType_autoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been
-- submitted, after which the assignment is considered Approved
-- automatically unless the Requester explicitly rejects it.
--
-- 'qualificationRequirements', 'createHITType_qualificationRequirements' - Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
--
-- 'keywords', 'createHITType_keywords' - One or more words or phrases that describe the HIT, separated by commas.
-- These words are used in searches to find HITs.
--
-- 'assignmentDurationInSeconds', 'createHITType_assignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT
-- after accepting it. If a Worker does not complete the assignment within
-- the specified duration, the assignment is considered abandoned. If the
-- HIT is still active (that is, its lifetime has not elapsed), the
-- assignment becomes available for other users to find and accept.
--
-- 'reward', 'createHITType_reward' - The amount of money the Requester will pay a Worker for successfully
-- completing the HIT.
--
-- 'title', 'createHITType_title' - The title of the HIT. A title should be short and descriptive about the
-- kind of task the HIT contains. On the Amazon Mechanical Turk web site,
-- the HIT title appears in search results, and everywhere the HIT is
-- mentioned.
--
-- 'description', 'createHITType_description' - A general description of the HIT. A description includes detailed
-- information about the kind of task the HIT contains. On the Amazon
-- Mechanical Turk web site, the HIT description appears in the expanded
-- view of search results, and in the HIT and assignment screens. A good
-- description gives the user enough information to evaluate the HIT before
-- accepting it.
newCreateHITType ::
  -- | 'assignmentDurationInSeconds'
  Prelude.Integer ->
  -- | 'reward'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateHITType
newCreateHITType
  pAssignmentDurationInSeconds_
  pReward_
  pTitle_
  pDescription_ =
    CreateHITType'
      { autoApprovalDelayInSeconds =
          Prelude.Nothing,
        qualificationRequirements = Prelude.Nothing,
        keywords = Prelude.Nothing,
        assignmentDurationInSeconds =
          pAssignmentDurationInSeconds_,
        reward = pReward_,
        title = pTitle_,
        description = pDescription_
      }

-- | The number of seconds after an assignment for the HIT has been
-- submitted, after which the assignment is considered Approved
-- automatically unless the Requester explicitly rejects it.
createHITType_autoApprovalDelayInSeconds :: Lens.Lens' CreateHITType (Prelude.Maybe Prelude.Integer)
createHITType_autoApprovalDelayInSeconds = Lens.lens (\CreateHITType' {autoApprovalDelayInSeconds} -> autoApprovalDelayInSeconds) (\s@CreateHITType' {} a -> s {autoApprovalDelayInSeconds = a} :: CreateHITType)

-- | Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
createHITType_qualificationRequirements :: Lens.Lens' CreateHITType (Prelude.Maybe [QualificationRequirement])
createHITType_qualificationRequirements = Lens.lens (\CreateHITType' {qualificationRequirements} -> qualificationRequirements) (\s@CreateHITType' {} a -> s {qualificationRequirements = a} :: CreateHITType) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more words or phrases that describe the HIT, separated by commas.
-- These words are used in searches to find HITs.
createHITType_keywords :: Lens.Lens' CreateHITType (Prelude.Maybe Prelude.Text)
createHITType_keywords = Lens.lens (\CreateHITType' {keywords} -> keywords) (\s@CreateHITType' {} a -> s {keywords = a} :: CreateHITType)

-- | The amount of time, in seconds, that a Worker has to complete the HIT
-- after accepting it. If a Worker does not complete the assignment within
-- the specified duration, the assignment is considered abandoned. If the
-- HIT is still active (that is, its lifetime has not elapsed), the
-- assignment becomes available for other users to find and accept.
createHITType_assignmentDurationInSeconds :: Lens.Lens' CreateHITType Prelude.Integer
createHITType_assignmentDurationInSeconds = Lens.lens (\CreateHITType' {assignmentDurationInSeconds} -> assignmentDurationInSeconds) (\s@CreateHITType' {} a -> s {assignmentDurationInSeconds = a} :: CreateHITType)

-- | The amount of money the Requester will pay a Worker for successfully
-- completing the HIT.
createHITType_reward :: Lens.Lens' CreateHITType Prelude.Text
createHITType_reward = Lens.lens (\CreateHITType' {reward} -> reward) (\s@CreateHITType' {} a -> s {reward = a} :: CreateHITType)

-- | The title of the HIT. A title should be short and descriptive about the
-- kind of task the HIT contains. On the Amazon Mechanical Turk web site,
-- the HIT title appears in search results, and everywhere the HIT is
-- mentioned.
createHITType_title :: Lens.Lens' CreateHITType Prelude.Text
createHITType_title = Lens.lens (\CreateHITType' {title} -> title) (\s@CreateHITType' {} a -> s {title = a} :: CreateHITType)

-- | A general description of the HIT. A description includes detailed
-- information about the kind of task the HIT contains. On the Amazon
-- Mechanical Turk web site, the HIT description appears in the expanded
-- view of search results, and in the HIT and assignment screens. A good
-- description gives the user enough information to evaluate the HIT before
-- accepting it.
createHITType_description :: Lens.Lens' CreateHITType Prelude.Text
createHITType_description = Lens.lens (\CreateHITType' {description} -> description) (\s@CreateHITType' {} a -> s {description = a} :: CreateHITType)

instance Prelude.AWSRequest CreateHITType where
  type Rs CreateHITType = CreateHITTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHITTypeResponse'
            Prelude.<$> (x Prelude..?> "HITTypeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHITType

instance Prelude.NFData CreateHITType

instance Prelude.ToHeaders CreateHITType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.CreateHITType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateHITType where
  toJSON CreateHITType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AutoApprovalDelayInSeconds" Prelude..=)
              Prelude.<$> autoApprovalDelayInSeconds,
            ("QualificationRequirements" Prelude..=)
              Prelude.<$> qualificationRequirements,
            ("Keywords" Prelude..=) Prelude.<$> keywords,
            Prelude.Just
              ( "AssignmentDurationInSeconds"
                  Prelude..= assignmentDurationInSeconds
              ),
            Prelude.Just ("Reward" Prelude..= reward),
            Prelude.Just ("Title" Prelude..= title),
            Prelude.Just ("Description" Prelude..= description)
          ]
      )

instance Prelude.ToPath CreateHITType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateHITType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHITTypeResponse' smart constructor.
data CreateHITTypeResponse = CreateHITTypeResponse'
  { -- | The ID of the newly registered HIT type.
    hITTypeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateHITTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITTypeId', 'createHITTypeResponse_hITTypeId' - The ID of the newly registered HIT type.
--
-- 'httpStatus', 'createHITTypeResponse_httpStatus' - The response's http status code.
newCreateHITTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHITTypeResponse
newCreateHITTypeResponse pHttpStatus_ =
  CreateHITTypeResponse'
    { hITTypeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the newly registered HIT type.
createHITTypeResponse_hITTypeId :: Lens.Lens' CreateHITTypeResponse (Prelude.Maybe Prelude.Text)
createHITTypeResponse_hITTypeId = Lens.lens (\CreateHITTypeResponse' {hITTypeId} -> hITTypeId) (\s@CreateHITTypeResponse' {} a -> s {hITTypeId = a} :: CreateHITTypeResponse)

-- | The response's http status code.
createHITTypeResponse_httpStatus :: Lens.Lens' CreateHITTypeResponse Prelude.Int
createHITTypeResponse_httpStatus = Lens.lens (\CreateHITTypeResponse' {httpStatus} -> httpStatus) (\s@CreateHITTypeResponse' {} a -> s {httpStatus = a} :: CreateHITTypeResponse)

instance Prelude.NFData CreateHITTypeResponse
