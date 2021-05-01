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
-- Module      : Network.AWS.AlexaBusiness.PutRoomSkillParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room skill parameter details by room, skill, and parameter key
-- ID. Not all skills have a room skill parameter.
module Network.AWS.AlexaBusiness.PutRoomSkillParameter
  ( -- * Creating a Request
    PutRoomSkillParameter (..),
    newPutRoomSkillParameter,

    -- * Request Lenses
    putRoomSkillParameter_roomArn,
    putRoomSkillParameter_skillId,
    putRoomSkillParameter_roomSkillParameter,

    -- * Destructuring the Response
    PutRoomSkillParameterResponse (..),
    newPutRoomSkillParameterResponse,

    -- * Response Lenses
    putRoomSkillParameterResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRoomSkillParameter' smart constructor.
data PutRoomSkillParameter = PutRoomSkillParameter'
  { -- | The ARN of the room associated with the room skill parameter. Required.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the skill associated with the room skill parameter. Required.
    skillId :: Prelude.Text,
    -- | The updated room skill parameter. Required.
    roomSkillParameter :: RoomSkillParameter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRoomSkillParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'putRoomSkillParameter_roomArn' - The ARN of the room associated with the room skill parameter. Required.
--
-- 'skillId', 'putRoomSkillParameter_skillId' - The ARN of the skill associated with the room skill parameter. Required.
--
-- 'roomSkillParameter', 'putRoomSkillParameter_roomSkillParameter' - The updated room skill parameter. Required.
newPutRoomSkillParameter ::
  -- | 'skillId'
  Prelude.Text ->
  -- | 'roomSkillParameter'
  RoomSkillParameter ->
  PutRoomSkillParameter
newPutRoomSkillParameter
  pSkillId_
  pRoomSkillParameter_ =
    PutRoomSkillParameter'
      { roomArn = Prelude.Nothing,
        skillId = pSkillId_,
        roomSkillParameter = pRoomSkillParameter_
      }

-- | The ARN of the room associated with the room skill parameter. Required.
putRoomSkillParameter_roomArn :: Lens.Lens' PutRoomSkillParameter (Prelude.Maybe Prelude.Text)
putRoomSkillParameter_roomArn = Lens.lens (\PutRoomSkillParameter' {roomArn} -> roomArn) (\s@PutRoomSkillParameter' {} a -> s {roomArn = a} :: PutRoomSkillParameter)

-- | The ARN of the skill associated with the room skill parameter. Required.
putRoomSkillParameter_skillId :: Lens.Lens' PutRoomSkillParameter Prelude.Text
putRoomSkillParameter_skillId = Lens.lens (\PutRoomSkillParameter' {skillId} -> skillId) (\s@PutRoomSkillParameter' {} a -> s {skillId = a} :: PutRoomSkillParameter)

-- | The updated room skill parameter. Required.
putRoomSkillParameter_roomSkillParameter :: Lens.Lens' PutRoomSkillParameter RoomSkillParameter
putRoomSkillParameter_roomSkillParameter = Lens.lens (\PutRoomSkillParameter' {roomSkillParameter} -> roomSkillParameter) (\s@PutRoomSkillParameter' {} a -> s {roomSkillParameter = a} :: PutRoomSkillParameter)

instance Prelude.AWSRequest PutRoomSkillParameter where
  type
    Rs PutRoomSkillParameter =
      PutRoomSkillParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRoomSkillParameterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRoomSkillParameter

instance Prelude.NFData PutRoomSkillParameter

instance Prelude.ToHeaders PutRoomSkillParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.PutRoomSkillParameter" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutRoomSkillParameter where
  toJSON PutRoomSkillParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoomArn" Prelude..=) Prelude.<$> roomArn,
            Prelude.Just ("SkillId" Prelude..= skillId),
            Prelude.Just
              ( "RoomSkillParameter"
                  Prelude..= roomSkillParameter
              )
          ]
      )

instance Prelude.ToPath PutRoomSkillParameter where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRoomSkillParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRoomSkillParameterResponse' smart constructor.
data PutRoomSkillParameterResponse = PutRoomSkillParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRoomSkillParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRoomSkillParameterResponse_httpStatus' - The response's http status code.
newPutRoomSkillParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRoomSkillParameterResponse
newPutRoomSkillParameterResponse pHttpStatus_ =
  PutRoomSkillParameterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRoomSkillParameterResponse_httpStatus :: Lens.Lens' PutRoomSkillParameterResponse Prelude.Int
putRoomSkillParameterResponse_httpStatus = Lens.lens (\PutRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@PutRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: PutRoomSkillParameterResponse)

instance Prelude.NFData PutRoomSkillParameterResponse
