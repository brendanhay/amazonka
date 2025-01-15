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
-- Module      : Amazonka.MechanicalTurk.CreateAdditionalAssignmentsForHIT
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MechanicalTurk.CreateAdditionalAssignmentsForHIT
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    CreateAdditionalAssignmentsForHIT
  where
  type
    AWSResponse CreateAdditionalAssignmentsForHIT =
      CreateAdditionalAssignmentsForHITResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAdditionalAssignmentsForHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateAdditionalAssignmentsForHIT
  where
  hashWithSalt
    _salt
    CreateAdditionalAssignmentsForHIT' {..} =
      _salt
        `Prelude.hashWithSalt` uniqueRequestToken
        `Prelude.hashWithSalt` hITId
        `Prelude.hashWithSalt` numberOfAdditionalAssignments

instance
  Prelude.NFData
    CreateAdditionalAssignmentsForHIT
  where
  rnf CreateAdditionalAssignmentsForHIT' {..} =
    Prelude.rnf uniqueRequestToken `Prelude.seq`
      Prelude.rnf hITId `Prelude.seq`
        Prelude.rnf numberOfAdditionalAssignments

instance
  Data.ToHeaders
    CreateAdditionalAssignmentsForHIT
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateAdditionalAssignmentsForHIT
  where
  toJSON CreateAdditionalAssignmentsForHIT' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UniqueRequestToken" Data..=)
              Prelude.<$> uniqueRequestToken,
            Prelude.Just ("HITId" Data..= hITId),
            Prelude.Just
              ( "NumberOfAdditionalAssignments"
                  Data..= numberOfAdditionalAssignments
              )
          ]
      )

instance
  Data.ToPath
    CreateAdditionalAssignmentsForHIT
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateAdditionalAssignmentsForHIT
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAdditionalAssignmentsForHITResponse' smart constructor.
data CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateAdditionalAssignmentsForHITResponse' {..} =
    Prelude.rnf httpStatus
