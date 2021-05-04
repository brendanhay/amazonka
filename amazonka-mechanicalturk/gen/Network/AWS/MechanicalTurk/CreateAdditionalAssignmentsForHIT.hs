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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
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
    uniqueRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HIT to extend.
    hITId :: Prelude.Text,
    -- | The number of additional assignments to request for this HIT.
    numberOfAdditionalAssignments :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'numberOfAdditionalAssignments'
  Prelude.Int ->
  CreateAdditionalAssignmentsForHIT
newCreateAdditionalAssignmentsForHIT
  pHITId_
  pNumberOfAdditionalAssignments_ =
    CreateAdditionalAssignmentsForHIT'
      { uniqueRequestToken =
          Prelude.Nothing,
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
createAdditionalAssignmentsForHIT_uniqueRequestToken :: Lens.Lens' CreateAdditionalAssignmentsForHIT (Prelude.Maybe Prelude.Text)
createAdditionalAssignmentsForHIT_uniqueRequestToken = Lens.lens (\CreateAdditionalAssignmentsForHIT' {uniqueRequestToken} -> uniqueRequestToken) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {uniqueRequestToken = a} :: CreateAdditionalAssignmentsForHIT)

-- | The ID of the HIT to extend.
createAdditionalAssignmentsForHIT_hITId :: Lens.Lens' CreateAdditionalAssignmentsForHIT Prelude.Text
createAdditionalAssignmentsForHIT_hITId = Lens.lens (\CreateAdditionalAssignmentsForHIT' {hITId} -> hITId) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {hITId = a} :: CreateAdditionalAssignmentsForHIT)

-- | The number of additional assignments to request for this HIT.
createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments :: Lens.Lens' CreateAdditionalAssignmentsForHIT Prelude.Int
createAdditionalAssignmentsForHIT_numberOfAdditionalAssignments = Lens.lens (\CreateAdditionalAssignmentsForHIT' {numberOfAdditionalAssignments} -> numberOfAdditionalAssignments) (\s@CreateAdditionalAssignmentsForHIT' {} a -> s {numberOfAdditionalAssignments = a} :: CreateAdditionalAssignmentsForHIT)

instance
  Prelude.AWSRequest
    CreateAdditionalAssignmentsForHIT
  where
  type
    Rs CreateAdditionalAssignmentsForHIT =
      CreateAdditionalAssignmentsForHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAdditionalAssignmentsForHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateAdditionalAssignmentsForHIT

instance
  Prelude.NFData
    CreateAdditionalAssignmentsForHIT

instance
  Prelude.ToHeaders
    CreateAdditionalAssignmentsForHIT
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    CreateAdditionalAssignmentsForHIT
  where
  toJSON CreateAdditionalAssignmentsForHIT' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UniqueRequestToken" Prelude..=)
              Prelude.<$> uniqueRequestToken,
            Prelude.Just ("HITId" Prelude..= hITId),
            Prelude.Just
              ( "NumberOfAdditionalAssignments"
                  Prelude..= numberOfAdditionalAssignments
              )
          ]
      )

instance
  Prelude.ToPath
    CreateAdditionalAssignmentsForHIT
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateAdditionalAssignmentsForHIT
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAdditionalAssignmentsForHITResponse' smart constructor.
data CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateAdditionalAssignmentsForHITResponse
newCreateAdditionalAssignmentsForHITResponse
  pHttpStatus_ =
    CreateAdditionalAssignmentsForHITResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createAdditionalAssignmentsForHITResponse_httpStatus :: Lens.Lens' CreateAdditionalAssignmentsForHITResponse Prelude.Int
createAdditionalAssignmentsForHITResponse_httpStatus = Lens.lens (\CreateAdditionalAssignmentsForHITResponse' {httpStatus} -> httpStatus) (\s@CreateAdditionalAssignmentsForHITResponse' {} a -> s {httpStatus = a} :: CreateAdditionalAssignmentsForHITResponse)

instance
  Prelude.NFData
    CreateAdditionalAssignmentsForHITResponse
