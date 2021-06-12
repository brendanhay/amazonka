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
-- Module      : Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateAdditionalAssignmentsForHIT@ operation increases the maximum
-- number of assignments of an existing HIT.
--
-- To extend the maximum number of assignments, specify the number of
-- additional assignments.
--
-- -   HITs created with fewer than 10 assignments cannot be extended to
--     have 10 or more assignments. Attempting to add assignments in a way
--     that brings the total number of assignments for a HIT from fewer
--     than 10 assignments to 10 or more assignments will result in an
--     @AWS.MechanicalTurk.InvalidMaximumAssignmentsIncrease@ exception.
--
-- -   HITs that were created before July 22, 2015 cannot be extended.
--     Attempting to extend HITs that were created before July 22, 2015
--     will result in an @AWS.MechanicalTurk.HITTooOldForExtension@
--     exception.
module Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
  ( -- * Creating a Request
    CreateAdditionalAssignmentsForHIT (..),
    newCreateAdditionalAssignmentsForHIT,

    -- * Request Lenses
    createAdditionalAssignmentsForHIT_uniqueRequestToken,
    createAdditionalAssignmentsForHIT_hITId,
    createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments,

    -- * Destructuring the Response
    CreateAdditionalAssignmentsForHITResponse (..),
    newCreateAdditionalAssignmentsForHITResponse,

    -- * Response Lenses
    createAdditionalAssignmentsForHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAdditionalAssignmentsForHIT' smart constructor.
data CreateAdditionalAssignmentsForHIT = CreateAdditionalAssignmentsForHIT'
  { -- | A unique identifier for this request, which allows you to retry the call
    -- on error without extending the HIT multiple times. This is useful in
    -- cases such as network timeouts where it is unclear whether or not the
    -- call succeeded on the server. If the extend HIT already exists in the
    -- system from a previous call using the same @UniqueRequestToken@,
    -- subsequent calls will return an error with a message containing the
    -- request ID.
    uniqueRequestToken :: Core.Maybe Core.Text,
    -- | The ID of the HIT to extend.
    hITId :: Core.Text,
    -- | The number of additional assignments to request for this HIT.
    numberOfAdditionalAssignments :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAdditionalAssignmentsForHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uniqueRequestToken', 'createAdditionalAssignmentsForHIT_uniqueRequestToken' - A unique identifier for this request, which allows you to retry the call
-- on error without extending the HIT multiple times. This is useful in
-- cases such as network timeouts where it is unclear whether or not the
-- call succeeded on the server. If the extend HIT already exists in the
-- system from a previous call using the same @UniqueRequestToken@,
-- subsequent calls will return an error with a message containing the
-- request ID.
--
-- 'hITId', 'createAdditionalAssignmentsForHIT_hITId' - The ID of the HIT to extend.
--
-- 'numberOfAdditionalAssignments', 'createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments' - The number of additional assignments to request for this HIT.
newCreateAdditionalAssignmentsForHIT ::
  -- | 'hITId'
  Core.Text ->
  -- | 'numberOfAdditionalAssignments'
  Core.Int ->
  CreateAdditionalAssignmentsForHIT
newCreateAdditionalAssignmentsForHIT
  pHITId_
  pNumberOfAdditionalAssignments_ =
    CreateAdditionalAssignmentsForHIT'
      { uniqueRequestToken =
          Core.Nothing,
        hITId = pHITId_,
        numberOfAdditionalAssignments =
          pNumberOfAdditionalAssignments_
      }

-- | A unique identifier for this request, which allows you to retry the call
-- on error without extending the HIT multiple times. This is useful in
-- cases such as network timeouts where it is unclear whether or not the
-- call succeeded on the server. If the extend HIT already exists in the
-- system from a previous call using the same @UniqueRequestToken@,
-- subsequent calls will return an error with a message containing the
-- request ID.
createAdditionalAssignmentsForHIT_uniqueRequestToken :: Lens.Lens' CreateAdditionalAssignmentsForHIT (Core.Maybe Core.Text)
createAdditionalAssignmentsForHIT_uniqueRequestToken = Lens.lens (\CreateAdditionalAssignmentsForHIT' {uniqueRequestToken} -> uniqueRequestToken) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {uniqueRequestToken = a} :: CreateAdditionalAssignmentsForHIT)

-- | The ID of the HIT to extend.
createAdditionalAssignmentsForHIT_hITId :: Lens.Lens' CreateAdditionalAssignmentsForHIT Core.Text
createAdditionalAssignmentsForHIT_hITId = Lens.lens (\CreateAdditionalAssignmentsForHIT' {hITId} -> hITId) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {hITId = a} :: CreateAdditionalAssignmentsForHIT)

-- | The number of additional assignments to request for this HIT.
createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments :: Lens.Lens' CreateAdditionalAssignmentsForHIT Core.Int
createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments = Lens.lens (\CreateAdditionalAssignmentsForHIT' {numberOfAdditionalAssignments} -> numberOfAdditionalAssignments) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {numberOfAdditionalAssignments = a} :: CreateAdditionalAssignmentsForHIT)

instance
  Core.AWSRequest
    CreateAdditionalAssignmentsForHIT
  where
  type
    AWSResponse CreateAdditionalAssignmentsForHIT =
      CreateAdditionalAssignmentsForHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAdditionalAssignmentsForHITResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateAdditionalAssignmentsForHIT

instance
  Core.NFData
    CreateAdditionalAssignmentsForHIT

instance
  Core.ToHeaders
    CreateAdditionalAssignmentsForHIT
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    CreateAdditionalAssignmentsForHIT
  where
  toJSON CreateAdditionalAssignmentsForHIT' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UniqueRequestToken" Core..=)
              Core.<$> uniqueRequestToken,
            Core.Just ("HITId" Core..= hITId),
            Core.Just
              ( "NumberOfAdditionalAssignments"
                  Core..= numberOfAdditionalAssignments
              )
          ]
      )

instance
  Core.ToPath
    CreateAdditionalAssignmentsForHIT
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateAdditionalAssignmentsForHIT
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAdditionalAssignmentsForHITResponse' smart constructor.
data CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAdditionalAssignmentsForHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAdditionalAssignmentsForHITResponse_httpStatus' - The response's http status code.
newCreateAdditionalAssignmentsForHITResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateAdditionalAssignmentsForHITResponse
newCreateAdditionalAssignmentsForHITResponse
  pHttpStatus_ =
    CreateAdditionalAssignmentsForHITResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createAdditionalAssignmentsForHITResponse_httpStatus :: Lens.Lens' CreateAdditionalAssignmentsForHITResponse Core.Int
createAdditionalAssignmentsForHITResponse_httpStatus = Lens.lens (\CreateAdditionalAssignmentsForHITResponse' {httpStatus} -> httpStatus) (\s@CreateAdditionalAssignmentsForHITResponse' {} a -> s {httpStatus = a} :: CreateAdditionalAssignmentsForHITResponse)

instance
  Core.NFData
    CreateAdditionalAssignmentsForHITResponse
